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
\ Fail closed: a non-elementwise region (LEW-NOTEW), an op not in the v1 chain set
\ (LEW-OP), more than 4 region inputs (LEW-INPUTS, v1 cap), a region without exactly
\ one materialized output (LEW-MULTIOUT), and any region input whose element count is
\ not the full N i.e. a broadcast operand (LEW-BCAST) are named throws. The analysis
\ runs BEFORE any PTX is emitted, so a rejected region emits nothing. maki -> habu
\ only; lower-ew owns -5170..-5176. Load after the cg emit stack (see requires).

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

-5170 constant E-LEW-NOTEW    \ region class is not pure elementwise
-5171 constant E-LEW-OP       \ op in the region chain is not a v1-supported elementwise op
-5172 constant E-LEW-INPUTS   \ region has more than the v1 input cap (4)
-5173 constant E-LEW-MULTIOUT \ region does not have exactly one materialized output
-5174 constant E-LEW-BCAST    \ a region input is not the full region shape (broadcast, v1)
-5175 constant E-LEW-REG      \ node/input register-map index out of range
-5176 constant E-LEW-CAP      \ driver text buffer capacity exceeded

package MAKI
private

4   constant LEW-MAX-IN       \ v1 input cap (documented): >4 region inputs fails closed
128 constant LEW-NCAP         \ node register-map size (mirrors model-ir MIR-CAP)

create LEW-INS    LEW-MAX-IN cells allot   \ ordered external-input operand refs
create LEW-IN-REG LEW-MAX-IN cells allot   \ loaded %f tile register per external input
create LEW-NODE-REG LEW-NCAP cells allot   \ result %f register per region-member node
variable LEW-NIN                            \ region input count
variable LEW-OUTNODE                        \ the single materialized region-output node
variable LEW-RID                            \ region id being lowered

\ ---- region membership + class ---------------------------------------------
: LEW-IN-REGION? ( n n -- bool )  swap FP-RID@ = ;        \ node rid -- in-region?
: LEW-EW-ONLY? ( n -- bool )  FP-REGION-CLASSMIX  1 CLASS-EW lshift  = ;

\ ---- supported op set (v1 elementwise chain) -------------------------------
: LEW-OP-OK? ( n -- bool ) {: op:n :}
   op OP-RELU = op OP-GELU = or op OP-SILU = or
   op OP-ADD = or op OP-MUL = or op OP-RESIDUAL-ADD = or ;
: LEW-CHECK-OPS ( n -- ) {: rid:n :}
   MIR-N@ 0 ?do
      i rid LEW-IN-REGION? if
         i MIR-OP@ LEW-OP-OK? 0= if E-LEW-OP throw then
      then
   loop ;

\ ---- external-input collection (first-appearance order, capped) -------------
\ An operand ref is external to the region when it names a model-input slot or a
\ producer node living in another region (a materialized boundary). Region-interior
\ producers are register values, never kernel inputs.
: LEW-REF-EXTERNAL? ( n n -- bool ) {: rid:n ref:n :}
   ref MIR-REF-INPUT? if true exit then
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
: LEW-COLLECT-INS ( n -- ) {: rid:n :}
   0 LEW-NIN !
   MIR-N@ 0 ?do  i rid LEW-IN-REGION? if rid i LEW-SCAN-INS then  loop ;

\ ---- single materialized output --------------------------------------------
: LEW-MAT-COUNT ( n -- n ) {: rid:n :}
   0 MIR-N@ 0 ?do  i rid LEW-IN-REGION? i MIR-MAT@ and if 1+ then  loop ;
: LEW-FIND-OUT ( n -- ) {: rid:n :}
   rid LEW-MAT-COUNT 1 <> if E-LEW-MULTIOUT throw then
   -1 LEW-OUTNODE !
   MIR-N@ 0 ?do  i rid LEW-IN-REGION? i MIR-MAT@ and if i LEW-OUTNODE ! then  loop ;

\ ---- shape check (flat kernel: every buffer is exactly N elements) ----------
: LEW-OUT-ELEMS ( -- n )  LEW-OUTNODE @ dup MIR-ROWS@ swap MIR-COLS@ * ;
: LEW-REF-ELEMS ( n -- n ) {: ref:n :}
   ref MIR-REF-INPUT? if
      ref MIR-REF-SLOT dup MIR-SLOT-ROWS@ swap MIR-SLOT-COLS@ *
   else ref dup MIR-ROWS@ swap MIR-COLS@ * then ;
: LEW-CHECK-SHAPES ( -- )
   LEW-OUT-ELEMS {: n:n :}
   LEW-NIN @ 0 ?do
      LEW-INS i cells + @ LEW-REF-ELEMS n <> if E-LEW-BCAST throw then
   loop ;

public
: LEW-ANALYZE ( n -- ) {: rid:n :}
   rid FP-REGION-MEMBERS drop                   \ validates FP-BUILD ran + rid range
   rid LEW-RID !
   rid LEW-EW-ONLY? 0= if E-LEW-NOTEW throw then
   rid LEW-CHECK-OPS
   rid LEW-COLLECT-INS
   rid LEW-FIND-OUT
   LEW-CHECK-SHAPES ;

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
: LEW-LOADS ( n -- ) {: off:n :}                 \ load input i at the shared ctx offset
   LEW-NIN @ 0 ?do  i 1+ off EMIT-LOAD  LEW-IN-REG i cells + !  loop ;
: LEW-EMIT-NODE ( n -- ) {: nd:n :}
   nd MIR-OP@ case
      OP-RELU          of nd 0 LEW-OPREG EMIT-RELU  endof
      OP-GELU          of nd 0 LEW-OPREG EMIT-GELU  endof
      OP-SILU          of nd 0 LEW-OPREG EMIT-SILU  endof
      OP-ADD           of nd LEW-BINREGS EMIT-ADD   endof
      OP-RESIDUAL-ADD  of nd LEW-BINREGS EMIT-ADD   endof
      OP-MUL           of nd LEW-BINREGS EMIT-MUL   endof
      E-LEW-OP throw
   endcase
   nd LEW-NR! ;
: LEW-CHAIN ( -- )
   MIR-N@ 0 ?do  i LEW-RID @ LEW-IN-REGION? if i LEW-EMIT-NODE then  loop ;
: LEW-BODY ( -- )
   1 EMIT-GRID-CTX {: off:n :}                    \ span base ignored; off = index*4, bounds vs %r1
   off LEW-LOADS
   LEW-CHAIN
   LEW-OUTNODE @ LEW-NR@  LEW-NIN @ 1+  off  EMIT-STORE ;

public
\ LEW-EMIT prints the region's PTX module to the current PTX sink (stdout, or the
\ in-process capture buffer). Analysis first, so a rejected region emits nothing.
: LEW-EMIT ( n -- )
   LEW-ANALYZE
   PTX-MODULE{
      LEW-ENTRY  LEW-OPEN  LEW-PARAMS  LEW-RESET-REGS
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
: LEW-WRITE-DRIVER ( ptr u8 n n ptr u8 n -- ) {: ma:ptr mu:n rid:n pa:ptr pu:n :}
   LEW-D-RESET
   s" require maki/cad.f"       LEW-D+ LEW-D-NL
   s" require maki/lower-ew.f"  LEW-D+ LEW-D-NL
   s" package MAKI"             LEW-D+ LEW-D-NL      \ MODEL:/FP-BUILD/LEW-EMIT are MAKI publics
   ma mu                        LEW-D+ LEW-D-NL
   s" FP-BUILD"                 LEW-D+ LEW-D-NL
   rid LEW-D-INT  s"  LEW-EMIT" LEW-D+ LEW-D-NL
   s" end-package"              LEW-D+ LEW-D-NL
   pa pu  LEW-DRV LEW-DRV-U @  WRITE-ALL ;

end-package
