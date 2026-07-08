\ maki/lower-red.f - lower ONE row-reduce fusion region to a block-per-row PTX kernel.
\
\ CAD-PLAN sections 5/11, device leg slice 2. The reduction dual of maki/lower-ew.f.
\ Given the model-IR node table (maki/model-ir.f) and its fusion plan (maki/fusion-plan.f),
\ LRED-EMIT lowers a region whose class mix is ROW-REDUCE (optionally with LEADING
\ elementwise PROLOGUE ops and/or TRAILING elementwise EPILOGUE ops fused around the
\ single reduction node - exactly what FP-BUILD produces: bare LAYERNORM/RMSNORM/
\ SOFTMAX-ROW are classmix ROW-REDUCE, GELU->RMSNORM fuses the GELU prologue into the
\ same region, and RMSNORM->RELU fuses a RELU epilogue) to the PTX SOURCE of a
\ ONE-BLOCK-PER-ROW kernel over R rows x C cols: grid = R, block = 256, per-thread lane
\ = one column masked against k=C (%r1). Each region input is a full RxC matrix loaded
\ row-wise (cg-collective ROW-SPAN / ROW-LOAD), the elementwise chain runs per-lane in
\ registers, the reduction node runs its shared-memory block reduction (cg-collective
\ EMIT-BLOCK-SUM / EMIT-BLOCK-MAX), and the region output row is masked-stored (ROW-STORE).
\
\ Reduction bodies MIRROR the host references op-for-op so the device f32 output matches
\ F64>F32(host) under the section 11 reduction tolerance (maki/lower-golden.f):
\   RMSNORM   (maki/rmsnorm.f RMS-FWD): y = x / sqrt(mean(x^2)+eps)    - one BLOCK-SUM.
\   LAYERNORM (maki/layernorm.f LN-FWD): y = (x-mu)/sqrt(var+eps)      - two BLOCK-SUMs.
\   SOFTMAX-ROW (maki/softmax.f SM-FWD): y = exp(x-max)/sum            - BLOCK-MAX + BLOCK-SUM.
\ eps is the SAME LN-EPS / RMS-EPS the host uses (single source; required here). Inactive
\ lanes (tid>=k) seed ROW-LOAD's -inf and are discarded by the reduction identity + the
\ masked store, so their per-lane inf/nan never reaches an active result.
\
\ Emit-mode discipline mirrors lower-ew.f: each per-op emitter (lib/ptx/cg.f, cg-collective.f,
\ cg-activation.f) appends its PTX lines and returns a %f register number; PTX-MODULE{ writes
\ the one shared header. Kernel ABI (named REGION_<rid>): one `.param .u64` per region input,
\ then `.param .u64 p_out`, then `.param .u32 p_k` (= C, the row width -> %r1); shared SMEM
\ for the block reduction. Block is fixed at 256 (matches the launch grid).
\
\ BROADCAST operands (bias/scale around the reduction): a region input whose shape is a legal
\ ROW (1xC) or SCALAR (1x1) broadcast of the region shape is loaded with its row span pinned to
\ row 0 (EMIT-ROW-SPAN0), so every block reads the SAME single row - a 1xC reads element tid
\ (the shared column ctx), a 1x1 reads element 0 (a zero ctx). A COLUMN (Rx1) broadcast loads a
\ STRIDE-1 row span (EMIT-ROW-SPAN-STRIDE1: base + row*4) with a zero column ctx, so every lane
\ in block r reads element r. All three mirror the host executor EX-BC@ (maki/bcast.f). No legal
\ capture class produces an Rx1 into a reduction today (cad.f SHP-LEGAL?: BIAS 1xC, SCALE
\ 1x1/same), so the BC-COL lowering is defense-in-depth cover for hand-built/synthesized IR.
\ OP-BIAS lowers as EMIT-ADD and OP-SCALE as EMIT-MUL, matching the executor's scalar references.
\
\ Fail closed BEFORE any PTX is emitted (a rejected region emits nothing): a region whose
\ class mix is not ROW-REDUCE (optionally + EW) (LRED-NOTRED), an op that is neither a
\ v1 elementwise op nor a supported reduction (LRED-OP), a region with != 1 reduction node
\ (LRED-MULTIRED, v1 cap: one reduction per region), more than the v1 input cap (LRED-INPUTS),
\ a region whose materialized-output count is not exactly one (LRED-MULTIOUT - a defense-in-depth
\ PLANNER-INVARIANT guard, not a v1 cap: the planner always plans exactly one materialized output
\ per region, proven in maki/fusion-mout-test.f), a region input whose shape is not a legal
\ broadcast of the RxC region shape - a non-1-non-full dim - or a broadcast (non-RxC) input 0
\ (LRED-BODY hardwires the full data-operand row span for input 0)
\ (LRED-BCAST), and a row width beyond the block cap (LRED-COLS, v1: C <= 256) are named throws.
\ maki -> habu only; lower-red owns -5185..-5192. Load after the cg emit stack (see requires).

require lib/errors.f
require lib/string.f
require lib/float.f
require lib/fmt.f
require src/arch/ptx/emit.f
require lib/ptx/header.f
require lib/ptx/cg.f
require lib/ptx/cg-collective.f
require lib/ptx/cg-activation.f
require maki/op-kind.f
require maki/op-registry.f
require maki/model-ir.f
require maki/fusion-plan.f
require maki/move-view.f
require maki/bcast.f
require maki/layernorm.f
require maki/rmsnorm.f

-5185 constant E-LRED-NOTRED   \ region class mix is not a supported row-reduce region
-5186 constant E-LRED-OP       \ a region op is neither a v1 elementwise op nor a supported reduction
-5187 constant E-LRED-MULTIRED \ region does not have exactly one reduction node (v1 cap)
-5188 constant E-LRED-INPUTS   \ region has more than the v1 input cap (4)
-5189 constant E-LRED-MULTIOUT \ region's materialized-output count != 1 (planner-invariant guard, not a cap)
-5190 constant E-LRED-BCAST    \ a region input shape is not a legal broadcast (illegal dim / broadcast input 0)
-5191 constant E-LRED-COLS     \ row width (cols) exceeds the block cap (v1: k <= 256)
-5192 constant E-LRED-REG      \ node / input register-map index out of range

package MAKI
private

4    constant LRED-MAX-IN      \ v1 input cap (documented): >4 region inputs fails closed
128  constant LRED-NCAP        \ node register-map size (mirrors model-ir MIR-CAP)
256  constant LRED-BLOCK       \ launch block size (one block per row; k <= block)
1024 constant LRED-SMEM-BYTES  \ block reduction shared bytes (LRED-BLOCK * 4)

create LRED-INS     LRED-MAX-IN cells allot   \ ordered external-input operand refs
create LRED-IN-REG  LRED-MAX-IN cells allot   \ loaded %f tile register per external input
create LRED-IN-BC   LRED-MAX-IN cells allot   \ broadcast class per external input (maki/bcast.f)
create LRED-NODE-REG LRED-NCAP cells allot    \ result %f register per region-member node
variable LRED-NIN                              \ region input count
variable LRED-OUTNODE                          \ the single materialized region-output node
variable LRED-RID                              \ region id being lowered

\ ---- region membership + class ---------------------------------------------
: LRED-IN-REGION? ( n n -- bool )  swap FP-RID@ = ;      \ node rid -- in-region?

\ class mix must contain the ROW-REDUCE bit and nothing outside {EW, ROW-REDUCE, MOVEMENT};
\ a dissolved free-movement prologue (maki/move-view.f) folds into the row-span base offset.
: LRED-CLASS-OK? ( n -- bool ) {: rid:n :}
   rid FP-REGION-CLASSMIX {: mix:n :}
   mix 1 CLASS-ROW-REDUCE lshift and 0= if false exit then
   mix  1 CLASS-EW lshift  1 CLASS-ROW-REDUCE lshift or  1 CLASS-MOVEMENT lshift or
      invert and 0= ;

\ ---- supported op sets -----------------------------------------------------
: LRED-RED-OP? ( n -- bool ) {: op:n :}          \ a supported row reduction
   op OP-LAYERNORM = op OP-RMSNORM = or op OP-SOFTMAX-ROW = or ;
: LRED-EW-OP? ( n -- bool ) {: op:n :}           \ a v1 elementwise prologue / epilogue op
   op OP-RELU = op OP-GELU = or op OP-SILU = or
   op OP-ADD = or op OP-MUL = or op OP-RESIDUAL-ADD = or
   op OP-BIAS = or op OP-SCALE = or ;             \ broadcast binary prologue/epilogue (1xC / 1x1)

: LRED-RED-COUNT ( n -- n ) {: rid:n :}          \ reduction nodes in the region
   0 MIR-N@ 0 ?do  i rid LRED-IN-REGION? i MIR-OP@ LRED-RED-OP? and if 1+ then  loop ;

\ compute members must be a reduction or v1 EW op; movement members must be v1-foldable
\ dissolved movements (MVW-CHECK fails closed on staged / mat / non-slot source).
: LRED-CHECK-OPS ( n -- ) {: rid:n :}
   MIR-N@ 0 ?do
      i rid LRED-IN-REGION? if
         i MIR-MOVE? if i MVW-CHECK
         else
            i MIR-OP@ {: op:n :}
            op LRED-RED-OP? op LRED-EW-OP? or 0= if E-LRED-OP throw then
         then
      then
   loop
   rid LRED-RED-COUNT 1 <> if E-LRED-MULTIRED throw then ;

\ ---- external-input collection (first-appearance order, capped) -------------
: LRED-REF-EXTERNAL? ( n n -- bool ) {: rid:n ref:n :}
   ref MIR-REF-INPUT? if true exit then
   ref MVW-DISSOLVED? if true exit then       \ a folded movement node is a virtual input
   ref FP-RID@ rid <> ;
: LRED-INS-IDX ( n -- n ) {: ref:n :}            \ index in LRED-INS, or -1
   LRED-NIN @ 0 ?do  ref LRED-INS i cells + @ = if i unloop exit then  loop  -1 ;
: LRED-INS-ADD ( n -- ) {: ref:n :}
   ref LRED-INS-IDX -1 > if exit then            \ already recorded
   LRED-NIN @ LRED-MAX-IN >= if E-LRED-INPUTS throw then
   ref LRED-INS LRED-NIN @ cells + !  LRED-NIN @ 1+ LRED-NIN ! ;
: LRED-SCAN-INS ( n n -- ) {: rid:n nd:n :}
   nd MIR-IN-COUNT@ 0 ?do
      nd i MIR-IN@ {: ref:n :}
      rid ref LRED-REF-EXTERNAL? if ref LRED-INS-ADD then
   loop ;
\ only COMPUTE members contribute inputs; a movement member reaches the kernel as the
\ virtual movement-node input its consumer scans, not as a direct input.
: LRED-COLLECT-INS ( n -- ) {: rid:n :}
   0 LRED-NIN !
   MIR-N@ 0 ?do  i rid LRED-IN-REGION? i MIR-MOVE? 0= and if rid i LRED-SCAN-INS then  loop ;

\ ---- single materialized output --------------------------------------------
\ The planner plans EXACTLY ONE materialized output per region (linear operand-0 chain, the
\ tail its sole materialized node; proof + fan-out battery in maki/fusion-mout-test.f).
\ LRED-FIND-OUT asserts that invariant as defense-in-depth: != 1 is a corrupted plan (>1
\ structurally impossible, 0 a materialization-flag regression), never a v1 cap.
: LRED-MAT-COUNT ( n -- n ) {: rid:n :}
   0 MIR-N@ 0 ?do  i rid LRED-IN-REGION? i MIR-MAT@ and if 1+ then  loop ;
: LRED-FIND-OUT ( n -- ) {: rid:n :}
   rid LRED-MAT-COUNT 1 <> if E-LRED-MULTIOUT throw then
   -1 LRED-OUTNODE !
   MIR-N@ 0 ?do  i rid LRED-IN-REGION? i MIR-MAT@ and if i LRED-OUTNODE ! then  loop ;

\ ---- broadcast classification (row kernel: each input is full / row / scalar) --
: LRED-REF-ROWS ( n -- n ) {: ref:n :}
   ref MIR-REF-INPUT? if ref MIR-REF-SLOT MIR-SLOT-ROWS@ else ref MIR-ROWS@ then ;
: LRED-REF-COLS ( n -- n ) {: ref:n :}
   ref MIR-REF-INPUT? if ref MIR-REF-SLOT MIR-SLOT-COLS@ else ref MIR-COLS@ then ;
\ classify each input; FULL/ROW/SCALAR/COL load with the row loader (row 0 pinned for a
\ row/scalar broadcast, a stride-1 span for an Rx1 column); an illegal dim fails closed.
\ Input 0 must be the FULL data operand - LRED-BODY hardwires its full row span - so a
\ broadcast in position 0 also fails closed. Record the class for LRED-BODY's loads.
: LRED-CLASSIFY-INS ( -- )
   LRED-OUTNODE @ {: out:n :}
   out MIR-ROWS@ {: R:n :}  out MIR-COLS@ {: C:n :}
   LRED-NIN @ 0 ?do
      LRED-INS i cells + @ {: ref:n :}
      ref LRED-REF-ROWS  ref LRED-REF-COLS  R C  BC-CLASS {: cls:n :}
      cls BC-ILLEGAL = if E-LRED-BCAST throw then
      i 0=  cls BC-FULL <>  and if E-LRED-BCAST throw then
      cls LRED-IN-BC i cells + !
   loop ;

public
: LRED-ANALYZE ( n -- ) {: rid:n :}
   rid FP-REGION-MEMBERS drop                    \ validates FP-BUILD ran + rid range
   rid LRED-RID !
   rid LRED-CLASS-OK? 0= if E-LRED-NOTRED throw then
   rid LRED-CHECK-OPS
   rid LRED-COLLECT-INS
   rid LRED-FIND-OUT
   LRED-CLASSIFY-INS
   LRED-OUTNODE @ MIR-COLS@ LRED-BLOCK > if E-LRED-COLS throw then ;

\ ---- analysis accessors (lower-launch / lower-golden read these) ------------
: LRED-NIN@ ( -- n )      LRED-NIN @ ;
: LRED-IN-REF@ ( n -- n ) {: i:n :}
   i 0 < i LRED-NIN @ >= or if E-LRED-REG throw then  LRED-INS i cells + @ ;
: LRED-OUT-NODE@ ( -- n ) LRED-OUTNODE @ ;
: LRED-ROWS@ ( -- n )     LRED-OUTNODE @ MIR-ROWS@ ;
: LRED-COLS@ ( -- n )     LRED-OUTNODE @ MIR-COLS@ ;
: LRED-ELEMS ( -- n )     LRED-ROWS@ LRED-COLS@ * ;
: LRED-RID@ ( -- n )      LRED-RID @ ;

private

\ ---- register map (member node result reg + operand resolution) -------------
: LRED-NR@ ( n -- n ) {: nd:n :}
   nd 0 < nd LRED-NCAP >= or if E-LRED-REG throw then  LRED-NODE-REG nd cells + @ ;
: LRED-NR! ( n n -- ) {: r:n nd:n :}
   nd 0 < nd LRED-NCAP >= or if E-LRED-REG throw then  r LRED-NODE-REG nd cells + ! ;

: LRED-REF-REG ( n -- n ) {: ref:n :}
   ref LRED-INS-IDX {: k:n :}
   k -1 > if LRED-IN-REG k cells + @ exit then     \ external input: its loaded tile reg
   ref 0 < if E-LRED-REG throw then                \ input slot that is not a region input
   ref LRED-NR@ ;                                   \ interior producer: its result reg
: LRED-OPREG ( n n -- n )  MIR-IN@ LRED-REF-REG ;   \ node k -- reg
: LRED-BINREGS ( n -- n n ) {: nd:n :}  nd 0 LRED-OPREG  nd 1 LRED-OPREG ;

\ ---- reduction glue emitters (no collective covers these; use the cg primitives) --
: LRED-EMIT-COLS-F ( -- n )                      \ uniform f = float(k) from %r1
   CG-NEXT-F {: r:n :}
   SB-RESET s" cvt.rn.f32.u32 " CG-S r CG-F s" , %r1;" CG-S CG-LINE
   r ;
: LRED-EMIT-SQRT ( n -- n ) {: x:n :}            \ IEEE round-to-nearest f32 sqrt (mirrors host fsqrt)
   CG-NEXT-F {: r:n :}
   SB-RESET s" sqrt.rn.f32 " CG-S r CG-F s" , " CG-S x CG-F s" ;" CG-S CG-LINE
   r ;

\ ---- reduction bodies (mirror the host references op-for-op) ----------------
\ RMS-FWD: y = x / sqrt(mean(x^2) + eps)
: LRED-EMIT-RMS ( n -- n ) {: x:n :}
   x x EMIT-MUL {: x2:n :}
   x2 EMIT-BLOCK-SUM {: ssq:n :}
   ssq LRED-EMIT-COLS-F EMIT-U/ {: ms:n :}
   ms RMS-EPS EMIT-ADDC {: mse:n :}
   mse LRED-EMIT-SQRT {: rr:n :}
   x rr EMIT-B/ ;

\ LN-FWD: mu = mean(x) ; var = mean((x-mu)^2) ; y = (x-mu)/sqrt(var+eps)
: LRED-EMIT-LN ( n -- n ) {: x:n :}
   LRED-EMIT-COLS-F {: fk:n :}
   x EMIT-BLOCK-SUM fk EMIT-U/ {: mu:n :}
   x mu EMIT-B- {: d:n :}
   d d EMIT-MUL EMIT-BLOCK-SUM fk EMIT-U/ {: var:n :}
   var LN-EPS EMIT-ADDC {: vare:n :}
   vare LRED-EMIT-SQRT {: std:n :}
   d std EMIT-B/ ;

\ SM-FWD: mx = max(x) ; e = exp(x-mx) ; y = e / sum(e)
: LRED-EMIT-SM ( n -- n ) {: x:n :}
   x EMIT-BLOCK-MAX {: mx:n :}
   x mx EMIT-B- EMIT-EXP {: e:n :}
   e EMIT-BLOCK-SUM {: s:n :}
   e s EMIT-B/ ;

\ ---- per-node emit (elementwise prologue/epilogue, then the reduction) ------
: LRED-EMIT-NODE ( n -- ) {: nd:n :}
   nd MIR-OP@ case
      OP-RELU          of nd 0 LRED-OPREG EMIT-RELU     endof
      OP-GELU          of nd 0 LRED-OPREG EMIT-GELU     endof
      OP-SILU          of nd 0 LRED-OPREG EMIT-SILU     endof
      OP-ADD           of nd LRED-BINREGS EMIT-ADD      endof
      OP-RESIDUAL-ADD  of nd LRED-BINREGS EMIT-ADD      endof
      OP-BIAS          of nd LRED-BINREGS EMIT-ADD      endof
      OP-MUL           of nd LRED-BINREGS EMIT-MUL      endof
      OP-SCALE         of nd LRED-BINREGS EMIT-MUL      endof
      OP-LAYERNORM     of nd 0 LRED-OPREG LRED-EMIT-LN  endof
      OP-RMSNORM       of nd 0 LRED-OPREG LRED-EMIT-RMS endof
      OP-SOFTMAX-ROW   of nd 0 LRED-OPREG LRED-EMIT-SM  endof
      E-LRED-OP throw
   endcase
   nd LRED-NR! ;
: LRED-CHAIN ( -- )                              \ movement members emit no compute (folded)
   MIR-N@ 0 ?do  i LRED-RID @ LRED-IN-REGION? i MIR-MOVE? 0= and if i LRED-EMIT-NODE then  loop ;

\ fold each dissolved movement input into a base-pointer offset (reshape 0 / slice r0*cols*4):
\ the generic operand pointer is advanced before EMIT-ROW-SPAN cvta's it, so each block reads
\ the movement's source row window with no other change (maki/move-view.f).
: LRED-APPLY-VIEWS ( -- )
   LRED-NIN @ 0 ?do
      LRED-INS i cells + @ MVW-RESOLVE-OFF {: off:n :}
      off 0 > if
         SB-RESET s" add.u64 %rd" CG-S i 1+ SB-U s" , %rd" CG-S i 1+ SB-U s" , " CG-S off SB-U s" ;" CG-S CG-LINE
      then
   loop ;

\ ---- entry / regs / params scaffolding (K inputs + output + k) --------------
: LRED-KNAME ( -- )  s" REGION_" CG-S LRED-RID @ SB-U ;
: LRED-ENTRY ( -- )
   SB-RESET
   s" .visible .entry " CG-S LRED-KNAME s" (" CG-S
   LRED-NIN @ 0 ?do  s" .param .u64 p_in" CG-S i SB-U s" , " CG-S  loop
   s" .param .u64 p_out, .param .u32 p_k)" CG-S
   CG-LINE ;
: LRED-OPEN ( -- )
   s" {" PTX-L
   s" .reg .pred %p<64>;" PTX-L
   s" .reg .f32 %f<256>;" PTX-L
   s" .reg .b32 %r<128>;" PTX-L
   s" .reg .b64 %rd<64>;" PTX-L
   SB-RESET s" .shared .align 4 .b8 SMEM[" CG-S LRED-SMEM-BYTES SB-U s" ];" CG-S CG-LINE ;
: LRED-PARAMS ( -- )
   LRED-NIN @ 0 ?do
      SB-RESET s" ld.param.u64 %rd" CG-S i 1+ SB-U s" , [p_in" CG-S i SB-U s" ];" CG-S CG-LINE
   loop
   SB-RESET s" ld.param.u64 %rd" CG-S LRED-NIN @ 1+ SB-U s" , [p_out];" CG-S CG-LINE
   s" ld.param.u32 %r1, [p_k];" PTX-L ;
: LRED-RESET-REGS ( -- )                         \ counters after the K+1 param loads (k = %r1)
   1 CG-NF !  LRED-NIN @ 2 + CG-NRD !  2 CG-NR !  1 CG-NP !  0 CG-NL ! ;

\ ---- body: row ctx, per-input row loads, node chain, masked store -----------
: LRED-BASE ( n -- n )  1+ ;                     \ rd index of input i's pointer param (rd1..rdK)
: LRED-OUT-BASE ( -- n )  LRED-NIN @ 1+ ;        \ rd index of p_out (rd K+1)
\ per-input row load (i >= 1): FULL reads the r-th row span at the shared column ctx; a ROW (1xC)
\ broadcast pins the span to row 0 (same row every block) and reads element tid via the shared ctx;
\ a SCALAR (1x1) pins row 0 and reads element 0 via a zero ctx; a COLUMN (Rx1) reads element r in
\ every lane (stride-1 span at row r + a zero ctx = EX-BC@ [e/C]). Result tile -> LRED-IN-REG[i].
: LRED-LOAD-IN ( n n n -- ) {: i:n r:n ctx:n :}
   i cells LRED-IN-BC + @ case
      BC-FULL   of i LRED-BASE r EMIT-ROW-SPAN  ctx EMIT-ROW-LOAD          endof
      BC-ROW    of i LRED-BASE EMIT-ROW-SPAN0   ctx EMIT-ROW-LOAD          endof
      BC-SCALAR of i LRED-BASE EMIT-ROW-SPAN0   EMIT-ZERO-OFF EMIT-ROW-LOAD endof
      BC-COL    of i LRED-BASE r EMIT-ROW-SPAN-STRIDE1  EMIT-ZERO-OFF EMIT-ROW-LOAD endof
      drop E-LRED-BCAST throw
   endcase
   LRED-IN-REG i cells + ! ;
: LRED-BODY ( -- )
   EMIT-ROW {: r:n :}                            \ r = blockIdx.x (one block per row)
   0 LRED-BASE r EMIT-ROW-SPAN {: sp0:n :}       \ input-0 (always FULL data operand) row span
   sp0 EMIT-ROW-CTX {: ctx:n :}                  \ per-thread column byte offset (tid*4)
   sp0 ctx EMIT-ROW-LOAD  LRED-IN-REG 0 cells + !
   LRED-NIN @ 1 ?do  i r ctx LRED-LOAD-IN  loop
   LRED-CHAIN
   LRED-OUTNODE @ LRED-NR@  LRED-OUT-BASE r EMIT-ROW-SPAN  ctx EMIT-ROW-STORE ;

public
\ LRED-EMIT prints the region's PTX module to the current PTX sink (stdout, or the
\ in-process capture buffer). Analysis first, so a rejected region emits nothing.
: LRED-EMIT ( n -- )
   LRED-ANALYZE
   LRED-BLOCK %BLOCK                             \ block-per-row reduction schedule (matches launch)
   PTX-MODULE{
      LRED-ENTRY  LRED-OPEN  LRED-PARAMS  LRED-RESET-REGS
      LRED-APPLY-VIEWS
      LRED-BODY
      s" ret;" PTX-L
      s" }" PTX-L
   }PTX-MODULE ;

end-package
