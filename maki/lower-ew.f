\ maki/lower-ew.f - lower ONE elementwise fusion region to a flat PTX kernel.
\
\ CAD-PLAN sections 2/5/10, device leg slice 1. Given the current model-IR node
\ table (maki/model-ir.f) and its fusion plan (maki/fusion-plan.f), LEW-EMIT lowers a
\ chosen region whose class is PURE ELEMENTWISE (relu/gelu/silu/add/mul chains only -
\ no reductions, matmul, or movement) to the PTX SOURCE of a flat kernel over
\ N = rows*cols output elements: one masked coalesced load per region input operand,
\ the op chain applied in registers, one masked store of the region output.
\
\ Emit-mode discipline mirrors the existing cg drivers (tools/ptx/saxpy-cg.f): each
\ per-op emitter (lib/ptx/cg.f, cg-collective.f, cg-activation.f) is run to append its
\ PTX lines and return a %f register number; PTX-MODULE{ writes the one shared
\ .version/.target header. Kernel ABI (named REGION_<rid>): one `.param .u64` per
\ region input buffer, then `.param .u64 p_out`, then `.param .u32 p_n`; param
\ registers %rd1..%rd<K> (inputs), %rd<K+1> (output), %r1 (n). GELU/SILU mirror the
\ host references op-for-op so the device f32 output matches F64>F32(host) under the
\ section 11 tolerance (proven by maki/lower-golden.f LOWER-GOLDEN).
\
\ BROADCAST operands (bias/scale): a region input whose shape is a legal broadcast of the
\ region shape (1xC row, 1x1 scalar, Rx1 column) is loaded with a REMAPPED flat index that
\ mirrors the host executor EX-BC@ - 1xC reads [e mod C], 1x1 reads [0], Rx1 reads [e/C]
\ (maki/bcast.f classifies; lib/ptx/cg.f EMIT-MOD-OFF/ZERO-OFF/DIV-OFF emit the offset).
\ Broadcast inputs stay ordinary kernel inputs (no planner change): only their per-input
\ load offset differs from the shared coalesced offset. OP-BIAS lowers as EMIT-ADD and
\ OP-SCALE as EMIT-MUL, matching the executor's scalar references.
\
\ Fail closed: a non-elementwise region (LEW-NOTEW), an op not in the v1 chain set
\ (LEW-OP), more than 4 region inputs (LEW-INPUTS, v1 cap), a region whose materialized-
\ output count is not exactly one (LEW-MULTIOUT - a defense-in-depth PLANNER-INVARIANT guard,
\ not a v1 cap: the planner always plans exactly one materialized output per region, proven
\ over a fan-out battery in maki/fusion-mout-test.f), and any region input whose shape is not a legal
\ broadcast of the region shape - a dim that is neither 1 nor full (LEW-BCAST) - are named
\ throws. The analysis runs BEFORE any PTX is emitted, so a rejected region emits nothing.
\ maki -> habu only; lower-ew owns -5170..-5176. Load after the cg emit stack (see requires).

require lib/errors.f
require lib/string.f
require lib/float.f
require lib/fmt.f
require lib/fs.f
require lib/fs-mutate.f
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

-5170 constant E-LEW-NOTEW    \ region class is not pure elementwise
-5171 constant E-LEW-OP       \ op in the region chain is not a v1-supported elementwise op
-5172 constant E-LEW-INPUTS   \ region has more than the v1 input cap (4)
-5173 constant E-LEW-MULTIOUT \ region's materialized-output count != 1 (planner-invariant guard, not a cap)
-5174 constant E-LEW-BCAST    \ a region input shape is not a legal broadcast (a dim neither 1 nor full)
-5175 constant E-LEW-REG      \ node/input register-map index out of range
-5176 constant E-LEW-CAP      \ driver text buffer capacity exceeded

package MAKI
private

4   constant LEW-MAX-IN       \ v1 input cap (documented): >4 region inputs fails closed
128 constant LEW-NCAP         \ node register-map size (mirrors model-ir MIR-CAP)

create LEW-INS    LEW-MAX-IN cells allot   \ ordered external-input operand refs
create LEW-IN-REG LEW-MAX-IN cells allot   \ loaded %f tile register per external input
create LEW-IN-BC  LEW-MAX-IN cells allot   \ broadcast class per external input (maki/bcast.f)
create LEW-NODE-REG LEW-NCAP cells allot   \ result %f register per region-member node
variable LEW-NIN                            \ region input count
variable LEW-OUTNODE                        \ the single materialized region-output node
variable LEW-RID                            \ region id being lowered

\ ---- region membership + class ---------------------------------------------
: LEW-IN-REGION? ( n n -- bool )  swap FP-RID@ = ;        \ node rid -- in-region?
\ class mix must contain the EW bit and nothing outside {EW, MOVEMENT}: a dissolved
\ free-movement prologue (maki/move-view.f) folds into the flat kernel's load offset.
: LEW-EW-ONLY? ( n -- bool ) {: rid:n :}
   rid FP-REGION-CLASSMIX {: mix:n :}
   mix 1 CLASS-EW lshift and 0= if false exit then
   mix  1 CLASS-EW lshift  1 CLASS-MOVEMENT lshift or  invert and 0= ;

\ ---- supported op set (v1 elementwise chain) -------------------------------
\ BIAS/SCALE join the set as broadcast binary ops (their param operand is a 1xC / 1x1
\ broadcast); they lower to the same add.rn / mul.rn the executor uses (EX-EW2-EL).
: LEW-OP-OK? ( n -- bool ) {: op:n :}
   op OP-RELU = op OP-GELU = or op OP-SILU = or
   op OP-ADD = or op OP-MUL = or op OP-RESIDUAL-ADD = or
   op OP-BIAS = or op OP-SCALE = or ;
\ compute members must be v1 EW ops; movement members must be EW-foldable dissolved
\ movements (MVW-CHECK-EW folds a FREE offset OR a STAGED transpose, and fails closed on a
\ chained / mat / non-slot source before any emit).
: LEW-CHECK-OPS ( n -- ) {: rid:n :}
   MIR-N@ 0 ?do
      i rid LEW-IN-REGION? if
         i MIR-MOVE? if i MVW-CHECK-EW
         else i MIR-OP@ LEW-OP-OK? 0= if E-LEW-OP throw then then
      then
   loop ;

\ ---- external-input collection (first-appearance order, capped) -------------
\ An operand ref is external to the region when it names a model-input slot or a
\ producer node living in another region (a materialized boundary). Region-interior
\ producers are register values, never kernel inputs.
: LEW-REF-EXTERNAL? ( n n -- bool ) {: rid:n ref:n :}
   ref MIR-REF-INPUT? if true exit then
   ref MVW-DISSOLVED? if true exit then       \ a folded movement node is a virtual input
   ref FP-RID@ rid <> ;
: LEW-INS-IDX ( n -- n ) {: ref:n :}           \ index in LEW-INS, or -1
   LEW-NIN @ 0 ?do  ref LEW-INS i cells + @ = if i unloop exit then  loop  -1 ;
: LEW-INS-ADD ( n -- ) {: ref:n :}
   ref LEW-INS-IDX -1 > if exit then           \ already recorded
   LEW-NIN @ LEW-MAX-IN >= if E-LEW-INPUTS throw then
   ref LEW-INS LEW-NIN @ cells + !  LEW-NIN @ 1+ LEW-NIN ! ;
: LEW-SCAN-INS ( n n -- ) {: rid:n nd:n :}
   nd MIR-IN-COUNT@ 0 ?do
      nd i MIR-IN@ {: ref:n :}
      rid ref LEW-REF-EXTERNAL? if ref LEW-INS-ADD then
   loop ;
\ only COMPUTE members contribute inputs; a movement member's source reaches the kernel
\ as the virtual movement-node input its consumer scans, not as a direct input.
: LEW-COLLECT-INS ( n -- ) {: rid:n :}
   0 LEW-NIN !
   MIR-N@ 0 ?do  i rid LEW-IN-REGION? i MIR-MOVE? 0= and if rid i LEW-SCAN-INS then  loop ;

\ ---- single materialized output --------------------------------------------
\ The planner plans EXACTLY ONE materialized output per region (maki/fusion-plan.f grows a
\ linear operand-0 chain whose only materialized node is the tail; proof + fan-out battery in
\ maki/fusion-mout-test.f). LEW-FIND-OUT asserts that invariant as defense-in-depth: != 1 is a
\ corrupted plan (>1 structurally impossible, 0 a materialization-flag regression), never a v1 cap.
: LEW-MAT-COUNT ( n -- n ) {: rid:n :}
   0 MIR-N@ 0 ?do  i rid LEW-IN-REGION? i MIR-MAT@ and if 1+ then  loop ;
: LEW-FIND-OUT ( n -- ) {: rid:n :}
   rid LEW-MAT-COUNT 1 <> if E-LEW-MULTIOUT throw then
   -1 LEW-OUTNODE !
   MIR-N@ 0 ?do  i rid LEW-IN-REGION? i MIR-MAT@ and if i LEW-OUTNODE ! then  loop ;

\ ---- broadcast classification (flat kernel: each input reads a legal broadcast) -----
: LEW-OUT-ELEMS ( -- n )  LEW-OUTNODE @ dup MIR-ROWS@ swap MIR-COLS@ * ;
: LEW-ROWS ( -- n )  LEW-OUTNODE @ MIR-ROWS@ ;
: LEW-COLS ( -- n )  LEW-OUTNODE @ MIR-COLS@ ;
: LEW-REF-ROWS ( n -- n ) {: ref:n :}
   ref MIR-REF-INPUT? if ref MIR-REF-SLOT MIR-SLOT-ROWS@ else ref MIR-ROWS@ then ;
: LEW-REF-COLS ( n -- n ) {: ref:n :}
   ref MIR-REF-INPUT? if ref MIR-REF-SLOT MIR-SLOT-COLS@ else ref MIR-COLS@ then ;
\ classify each input's shape vs the region shape; an illegal (non-1-non-full dim) fails
\ closed, else record its broadcast class for the per-input load offset (LEW-LOAD-IN).
: LEW-CLASSIFY-INS ( -- )
   LEW-ROWS {: R:n :}  LEW-COLS {: C:n :}
   LEW-NIN @ 0 ?do
      LEW-INS i cells + @ {: ref:n :}
      ref LEW-REF-ROWS  ref LEW-REF-COLS  R C  BC-CLASS {: cls:n :}
      cls BC-ILLEGAL = if E-LEW-BCAST throw then
      cls LEW-IN-BC i cells + !
   loop ;

public
: LEW-ANALYZE ( n -- ) {: rid:n :}
   rid FP-REGION-MEMBERS drop                   \ validates FP-BUILD ran + rid range
   rid LEW-RID !
   rid LEW-EW-ONLY? 0= if E-LEW-NOTEW throw then
   rid LEW-CHECK-OPS
   rid LEW-COLLECT-INS
   rid LEW-FIND-OUT
   LEW-CLASSIFY-INS ;

\ ---- analysis accessors (lower-launch / lower-golden read these) ------------
: LEW-NIN@ ( -- n )      LEW-NIN @ ;
: LEW-IN-REF@ ( n -- n ) {: i:n :}
   i 0 < i LEW-NIN @ >= or if E-LEW-REG throw then  LEW-INS i cells + @ ;
: LEW-OUT-NODE@ ( -- n ) LEW-OUTNODE @ ;
: LEW-ELEMS ( -- n )     LEW-OUT-ELEMS ;
: LEW-RID@ ( -- n )      LEW-RID @ ;

private

\ ---- register map (member node result reg + operand resolution) -------------
: LEW-NR@ ( n -- n ) {: nd:n :}
   nd 0 < nd LEW-NCAP >= or if E-LEW-REG throw then  LEW-NODE-REG nd cells + @ ;
: LEW-NR! ( n n -- ) {: r:n nd:n :}
   nd 0 < nd LEW-NCAP >= or if E-LEW-REG throw then  r LEW-NODE-REG nd cells + ! ;

: LEW-REF-REG ( n -- n ) {: ref:n :}
   ref LEW-INS-IDX {: k:n :}
   k -1 > if LEW-IN-REG k cells + @ exit then     \ external input: its loaded tile reg
   ref 0 < if E-LEW-REG throw then                \ input slot that is not a region input
   ref LEW-NR@ ;                                   \ interior producer: its result reg
: LEW-OPREG  ( n n -- n )  MIR-IN@ LEW-REF-REG ;   \ node k -- reg
: LEW-BINREGS ( n -- n n ) {: nd:n :}  nd 0 LEW-OPREG  nd 1 LEW-OPREG ;

\ ---- entry / regs / params scaffolding (K inputs + output + n) --------------
: LEW-KNAME ( -- )  s" REGION_" CG-S LEW-RID @ SB-U ;
: LEW-ENTRY ( -- )
   SB-RESET
   s" .visible .entry " CG-S LEW-KNAME s" (" CG-S
   LEW-NIN @ 0 ?do  s" .param .u64 p_in" CG-S i SB-U s" , " CG-S  loop
   s" .param .u64 p_out, .param .u32 p_n)" CG-S
   CG-LINE ;
: LEW-OPEN ( -- )
   s" {" PTX-L
   s" .reg .pred %p<64>;" PTX-L
   s" .reg .f32 %f<256>;" PTX-L
   s" .reg .b32 %r<128>;" PTX-L
   s" .reg .b64 %rd<64>;" PTX-L ;
: LEW-PARAMS ( -- )
   LEW-NIN @ 0 ?do
      SB-RESET s" ld.param.u64 %rd" CG-S i 1+ SB-U s" , [p_in" CG-S i SB-U s" ];" CG-S CG-LINE
   loop
   SB-RESET s" ld.param.u64 %rd" CG-S LEW-NIN @ 1+ SB-U s" , [p_out];" CG-S CG-LINE
   s" ld.param.u32 %r1, [p_n];" PTX-L ;
: LEW-RESET-REGS ( -- )                          \ counters after the K+1 param loads (n = %r1)
   1 CG-NF !  LEW-NIN @ 2 + CG-NRD !  2 CG-NR !  1 CG-NP !  0 CG-NL ! ;

\ ---- body: grid ctx, per-input loads, op chain, store -----------------------
\ ctx byte-offset for a NON-STAGED input i: a FULL operand reads the shared coalesced offset
\ (its base was pre-advanced by LEW-APPLY-VIEWS); a broadcast operand reads the executor-matching
\ remap off the flat index - 1xC [e mod C], Rx1 [e/C], 1x1 [0] (maki/bcast.f).
: LEW-BC-OFF ( n n n -- n ) {: i:n off:n flatr:n :}
   i cells LEW-IN-BC + @ case
      BC-FULL   of off endof
      BC-ROW    of flatr LEW-COLS EMIT-MOD-OFF endof
      BC-COL    of flatr LEW-COLS EMIT-DIV-OFF endof
      BC-SCALAR of EMIT-ZERO-OFF endof
      drop E-LEW-BCAST throw
   endcase ;
\ Load input i: a STAGED transpose reads a per-element remapped offset off the flat index
\ (src_flat = (e mod dstC)*srcC + e/dstC), so its base stays at the slot origin; otherwise the
\ per-class broadcast/coalesced offset (LEW-BC-OFF) drives the load.
\ PERF: the transpose read is STRIDED (adjacent lanes stride by srcC), so folding trades a
\ materialization pass for uncoalesced loads; whether it wins over materialize-then-coalesced is
\ a device question (measure-before-believing, docs/eval-triton.md 3c) - goldens pending-zed.
: LEW-LOAD-IN ( n n n -- ) {: i:n off:n flatr:n :}
   LEW-INS i cells + @ {: ref:n :}
   ref MVW-STAGED? if
      ref MVW-XPOSE-DIMS {: dstc:n srcc:n :}
      flatr dstc srcc EMIT-XPOSE-OFF {: roff:n :}
      i 1+ roff EMIT-LOAD  LEW-IN-REG i cells + !
   else
      i off flatr LEW-BC-OFF {: ctxoff:n :}
      i 1+ ctxoff EMIT-LOAD  LEW-IN-REG i cells + !
   then ;
: LEW-LOADS ( n n -- ) {: off:n flatr:n :}       \ load each input at its resolved ctx offset
   LEW-NIN @ 0 ?do  i off flatr LEW-LOAD-IN  loop ;
: LEW-EMIT-NODE ( n -- ) {: nd:n :}
   nd MIR-OP@ case
      OP-RELU          of nd 0 LEW-OPREG EMIT-RELU  endof
      OP-GELU          of nd 0 LEW-OPREG EMIT-GELU  endof
      OP-SILU          of nd 0 LEW-OPREG EMIT-SILU  endof
      OP-ADD           of nd LEW-BINREGS EMIT-ADD   endof
      OP-RESIDUAL-ADD  of nd LEW-BINREGS EMIT-ADD   endof
      OP-BIAS          of nd LEW-BINREGS EMIT-ADD   endof
      OP-MUL           of nd LEW-BINREGS EMIT-MUL   endof
      OP-SCALE         of nd LEW-BINREGS EMIT-MUL   endof
      E-LEW-OP throw
   endcase
   nd LEW-NR! ;
: LEW-CHAIN ( -- )                               \ movement members emit no compute (folded)
   MIR-N@ 0 ?do  i LEW-RID @ LEW-IN-REGION? i MIR-MOVE? 0= and if i LEW-EMIT-NODE then  loop ;

\ fold each dissolved FREE movement input into a base-pointer offset (reshape 0 / slice
\ r0*cols*4): the generic operand pointer is advanced before EMIT-LOAD cvta's it, so the flat
\ kernel's load index reads the movement's source window with no other change (maki/move-view.f).
\ A STAGED transpose is a lane permutation, not a base offset, so it is skipped here and folded
\ per-element in LEW-LOAD-IN instead.
: LEW-APPLY-VIEWS ( -- )
   LEW-NIN @ 0 ?do
      LEW-INS i cells + @ {: ref:n :}
      ref MVW-STAGED? 0= if
         ref MVW-RESOLVE-OFF {: off:n :}
         off 0 > if
            SB-RESET s" add.u64 %rd" CG-S i 1+ SB-U s" , %rd" CG-S i 1+ SB-U s" , " CG-S off SB-U s" ;" CG-S CG-LINE
         then
      then
   loop ;
: LEW-BODY ( -- )
   1 EMIT-GRID-CTX {: off:n :}                    \ span base ignored; off = index*4, bounds vs %r1
   EMIT-GRID-INDEX {: flatr:n :}                  \ the flat output index reg (staged remap reads it)
   off flatr LEW-LOADS
   LEW-CHAIN
   LEW-OUTNODE @ LEW-NR@  LEW-NIN @ 1+  off  EMIT-STORE ;

public
\ LEW-EMIT prints the region's PTX module to the current PTX sink (stdout, or the
\ in-process capture buffer). Analysis first, so a rejected region emits nothing.
: LEW-EMIT ( n -- )
   LEW-ANALYZE
   PTX-MODULE{
      LEW-ENTRY  LEW-OPEN  LEW-PARAMS  LEW-RESET-REGS
      LEW-APPLY-VIEWS
      LEW-BODY
      CG-RET  CG-CLOSE
   }PTX-MODULE ;

private
\ ---- child emit driver ------------------------------------------------------
\ Emitting reads the in-process IR + plan, so a captured-PTX pipeline spawns a
\ fresh bin/hb that rebuilds the model and calls LEW-EMIT on stdout. LEW-WRITE-DRIVER
\ writes that driver: the model source, FP-BUILD, then "<rid> LEW-EMIT".
$2000 constant LEW-DRV-CAP
create LEW-DRV LEW-DRV-CAP allot  variable LEW-DRV-U
: LEW-D-RESET ( -- )  0 LEW-DRV-U ! ;
: LEW-D+ ( ptr u8 n -- ) {: a:ptr u:n :}
   LEW-DRV-U @ u + LEW-DRV-CAP > if E-LEW-CAP throw then
   a LEW-DRV LEW-DRV-U @ + u BYTE-COPY  LEW-DRV-U @ u + LEW-DRV-U ! ;
: LEW-D-C ( n -- ) {: c:n :}
   LEW-DRV-U @ 1+ LEW-DRV-CAP > if E-LEW-CAP throw then
   c LEW-DRV LEW-DRV-U @ + c!  LEW-DRV-U @ 1+ LEW-DRV-U ! ;
: LEW-D-NL ( -- )  $0A LEW-D-C ;
: LEW-D-INT ( n -- )  SB-RESET SB-INT SB$ LEW-D+ ;

public
\ Write a child driver that requires cad.f + `reqa/requ` (the owning lowering file),
\ defines the model `ma/mu`, builds the fusion plan, then runs `<rid> <emita/emitu>`
\ inside package MAKI (MODEL:/FP-BUILD/*-EMIT are MAKI publics), to the path `pa/pu`.
\ Both the elementwise (LEW-EMIT) and the row-reduce (LRED-EMIT) device legs spawn a
\ fresh bin/hb through this one writer.
: LOWER-DRIVER! ( ptr u8 n ptr u8 n ptr u8 n n ptr u8 n -- )
   {: ma:ptr mu:n reqa:ptr requ:n emita:ptr emitu:n rid:n pa:ptr pu:n :}
   LEW-D-RESET
   s" require maki/cad.f"  LEW-D+ LEW-D-NL
   reqa requ               LEW-D+ LEW-D-NL
   s" package MAKI"        LEW-D+ LEW-D-NL
   ma mu                   LEW-D+ LEW-D-NL
   s" FP-BUILD"            LEW-D+ LEW-D-NL
   rid LEW-D-INT  s"  "    LEW-D+  emita emitu LEW-D+ LEW-D-NL
   s" end-package"         LEW-D+ LEW-D-NL
   pa pu  LEW-DRV LEW-DRV-U @  WRITE-ALL ;

: LEW-WRITE-DRIVER ( ptr u8 n n ptr u8 n -- ) {: ma:ptr mu:n rid:n pa:ptr pu:n :}
   ma mu  s" require maki/lower-ew.f"  s" LEW-EMIT"  rid  pa pu  LOWER-DRIVER! ;

end-package
