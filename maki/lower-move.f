\ maki/lower-move.f - lower ONE materialized movement region to a copy PTX kernel.
\
\ CAD-PLAN 6.3 device leg, slice 4 (deliverable B). A movement node the planner could
\ NOT dissolve (MIR-MAT@ = 1: concat / gather / unaligned-slice by verdict, or a fan-out
\ transpose/slice that crosses a region boundary) is its own region and lowers to a COPY
\ kernel with remapped indices - the device dual of the maki/move.f host references
\ (MOVE-TRANSPOSE / MOVE-SLICE / MOVE-CONCAT / MOVE-GATHER). Movement carries NO compute
\ (NUM-EXACT), so each kernel is one f32 load + one f32 store per output element, differing
\ only in the source INDEX remap:
\   TRANSPOSE  dst[i,j]=src[j,i]   : coalesced write, strided read (i=e/R, j=e mod R, src=j*C+i)
\   SLICE      dst[k,c]=src[r0+k,c]: offset copy (src flat = e + r0*cols, baked p_a=r0*cols)
\   CONCAT     top A then B rows   : two-source copy (e<split -> A[e] ; else B[e-split])
\   GATHER     dst row k=src row id: indexed rows; the f32 index input is rounded EXACTLY
\              like the executor's EX-BUILD-IDX (x -> trunc(x+0.5)) via add.f32 +0.5 + cvt.rzi.
\
\ Kernel ABI (REGION_<rid>, uniform over the four): one/two `.param .u64 p_in<i>` buffers,
\ `.param .u64 p_out`, then `.param .u32 p_a, p_b, p_n`. The kind-specific scalars ride p_a/p_b
\ (transpose: p_a=src_rows R, p_b=src_cols C; slice: p_a=r0*cols; concat: p_a=split; gather:
\ p_a=cols C); p_n = the output element count. grid = ceil(N/256), block 256, one elem/thread,
\ masked against p_n. Gather's index input is buffer 1 (an f32 model input, like EX-BUILD-IDX).
\
\ Fail closed BEFORE any PTX (a rejected region emits nothing): a region that is not exactly one
\ materialized movement node (E-LMV-MULTI), a movement op outside the v1 copy set (E-LMV-OP), a
\ movement whose output the planner left un-materialized (E-LMV-NOOUT, the fusion-plan gap for a
\ free/staged movement that is a model output), a movement operand that is an un-materialized
\ interior node with no device buffer (E-LMV-INPUT; a slot or a cross-region MATERIALIZED producer
\ is fine - the whole-model run binds the producer buffer), and an operand/output above the launch arena
\ (E-LMV-DIMS) are named throws. maki -> habu only; lower-move owns -5205..-5212. Load after the
\ cg emit stack (see requires).

require lib/errors.f
require lib/string.f
require lib/fmt.f
require src/arch/ptx/emit.f
require lib/ptx/header.f
require lib/ptx/cg.f
require maki/op-kind.f
require maki/op-registry.f
require maki/model-ir.f
require maki/fusion-plan.f
require maki/move-facts.f

-5205 constant E-LMV-NOTMOVE  \ region is not a materialized movement region
-5206 constant E-LMV-MULTI    \ region is not exactly one movement node (v1 cap)
-5207 constant E-LMV-OP       \ movement op is not in the v1 copy set
-5208 constant E-LMV-INPUT    \ a movement operand is an un-materialized interior node (no device buffer)
-5209 constant E-LMV-DIMS     \ operand/output extent exceeds the launch arena cap
-5210 constant E-LMV-NOOUT    \ movement output left un-materialized (fusion-plan gap)
-5211 constant E-LMV-REG      \ input index out of range
-5212 constant E-LMV-STATE    \ accessor used before LMV-ANALYZE

package MAKI
private

2    constant LMV-MAX-IN      \ copy kernels read at most two buffers (concat A/B, gather src/idx)
4096 constant LMV-ARENA       \ v1 buffer element cap (mirrors lower-launch LLA-NCAP: 16 KB f32)

create LMV-INS   LMV-MAX-IN cells allot   \ ordered buffer-operand refs (must be input slots)
variable LMV-NIN                           \ buffer-operand count (1 or 2)
variable LMV-NODE                          \ the movement node
variable LMV-RID                           \ region id being lowered
variable LMV-PA  variable LMV-PB  variable LMV-PN   \ kernel u32 params
variable LMV-BUILT?

\ ---- region membership -----------------------------------------------------
: LMV-IN-REGION? ( n n -- bool )  swap FP-RID@ = ;      \ node rid -- in-region?
: LMV-COPY-OP? ( opkind -- bool )
   MATCH opkind
      transpose OF true ENDOF  slice OF true ENDOF  concat OF true ENDOF  gather OF true ENDOF
      add OF false ENDOF  mul OF false ENDOF  scale OF false ENDOF  bias OF false ENDOF
      relu OF false ENDOF  gelu OF false ENDOF  silu OF false ENDOF  layernorm OF false ENDOF
      rmsnorm OF false ENDOF  softmax-row OF false ENDOF  matmul OF false ENDOF  linear OF false ENDOF
      residual-add OF false ENDOF  cast OF false ENDOF  rope OF false ENDOF  reshape OF false ENDOF
      relu-bwd OF false ENDOF  gelu-bwd OF false ENDOF  silu-bwd OF false ENDOF
      layernorm-bwd OF false ENDOF  rmsnorm-bwd OF false ENDOF  softmax-row-bwd OF false ENDOF
      rope-bwd OF false ENDOF  rowsum-bwd OF false ENDOF  fullsum-dot-bwd OF false ENDOF
      pad-scatter OF false ENDOF  scatter-add OF false ENDOF
   ;MATCH ;

\ ---- operand-ref shape: a model input slot, or a MATERIALIZED producer node in another region
\ (slice 5 cross-region handoff - the whole-model run binds that producer's device buffer). A copy
\ region is exactly one movement node, so an operand can only be a slot or a cross-region node; an
\ un-materialized (interior) node would have no device buffer and fails closed (E-LMV-INPUT).
: LMV-REF-ROWS ( n -- n ) {: ref:n :}
   ref MIR-REF-INPUT? if  ref MIR-REF-SLOT MIR-SLOT-ROWS@  exit then
   ref MIR-MAT@ 0= if E-LMV-INPUT throw then  ref MIR-ROWS@ ;
: LMV-REF-COLS ( n -- n ) {: ref:n :}
   ref MIR-REF-INPUT? if  ref MIR-REF-SLOT MIR-SLOT-COLS@  exit then
   ref MIR-MAT@ 0= if E-LMV-INPUT throw then  ref MIR-COLS@ ;
: LMV-REF-ELEMS ( n -- n ) {: ref:n :}  ref LMV-REF-ROWS  ref LMV-REF-COLS * ;

\ ---- find the single materialized movement node ----------------------------
: LMV-MEMBERS ( n -- n ) {: rid:n :}                   \ region member count
   0 MIR-N@ 0 ?do  i rid LMV-IN-REGION? if 1+ then  loop ;
: LMV-FIND-NODE ( n -- ) {: rid:n :}
   rid LMV-MEMBERS 1 <> if E-LMV-MULTI throw then      \ v1: a copy region is one node
   -1 LMV-NODE !
   MIR-N@ 0 ?do  i rid LMV-IN-REGION? if i LMV-NODE ! then  loop
   LMV-NODE @ MIR-OP@ LMV-COPY-OP? 0= if E-LMV-OP throw then
   LMV-NODE @ MIR-MAT@ 0= if E-LMV-NOOUT throw then ;  \ planner must materialize the output

\ ---- collect buffer operands (transpose/slice: 1 ; concat/gather: 2) --------
: LMV-COLLECT-INS ( -- )
   LMV-NODE @ {: nd:n :}
   nd MIR-IN-COUNT@ {: c:n :}
   c LMV-MAX-IN > if E-LMV-REG throw then
   c LMV-NIN !
   c 0 ?do  nd i MIR-IN@ dup LMV-REF-ROWS drop  LMV-INS i cells + !  loop ;

\ ---- per-op derived dims + u32 params (p_a/p_b) -----------------------------
: LMV-DIMS-TRANSPOSE ( -- )                            \ p_a=src_rows R, p_b=src_cols C
   LMV-INS 0 cells + @ {: s:n :}
   s LMV-REF-ROWS LMV-PA !  s LMV-REF-COLS LMV-PB ! ;
: LMV-DIMS-SLICE ( -- )                                \ p_a = r0*cols (source-flat offset)
   LMV-NODE @ {: nd:n :}
   nd MIR-ATTR@ MV-PA@  nd MIR-COLS@ *  LMV-PA !  0 LMV-PB ! ;
: LMV-DIMS-CONCAT ( -- )                               \ p_a = split = A rows * A cols
   LMV-INS 0 cells + @ {: a:n :}
   a LMV-REF-ROWS  a LMV-REF-COLS *  LMV-PA !  0 LMV-PB ! ;
: LMV-DIMS-GATHER ( -- )                               \ p_a = cols C (of the source rows)
   LMV-INS 0 cells + @ LMV-REF-COLS LMV-PA !  0 LMV-PB ! ;
: LMV-SET-DIMS ( -- )
   LMV-NODE @ MIR-ROWS@ LMV-NODE @ MIR-COLS@ * LMV-PN !     \ N = output elems
   LMV-NODE @ MIR-OP@ MATCH opkind
      transpose OF LMV-DIMS-TRANSPOSE ENDOF
      slice     OF LMV-DIMS-SLICE     ENDOF
      concat    OF LMV-DIMS-CONCAT    ENDOF
      gather    OF LMV-DIMS-GATHER    ENDOF
      add OF E-LMV-OP throw ENDOF  mul OF E-LMV-OP throw ENDOF
      scale OF E-LMV-OP throw ENDOF  bias OF E-LMV-OP throw ENDOF
      relu OF E-LMV-OP throw ENDOF  gelu OF E-LMV-OP throw ENDOF
      silu OF E-LMV-OP throw ENDOF  layernorm OF E-LMV-OP throw ENDOF
      rmsnorm OF E-LMV-OP throw ENDOF  softmax-row OF E-LMV-OP throw ENDOF
      matmul OF E-LMV-OP throw ENDOF  linear OF E-LMV-OP throw ENDOF
      residual-add OF E-LMV-OP throw ENDOF  cast OF E-LMV-OP throw ENDOF
      rope OF E-LMV-OP throw ENDOF  reshape OF E-LMV-OP throw ENDOF
      relu-bwd OF E-LMV-OP throw ENDOF  gelu-bwd OF E-LMV-OP throw ENDOF
      silu-bwd OF E-LMV-OP throw ENDOF  layernorm-bwd OF E-LMV-OP throw ENDOF
      rmsnorm-bwd OF E-LMV-OP throw ENDOF  softmax-row-bwd OF E-LMV-OP throw ENDOF
      rope-bwd OF E-LMV-OP throw ENDOF  rowsum-bwd OF E-LMV-OP throw ENDOF
      fullsum-dot-bwd OF E-LMV-OP throw ENDOF  pad-scatter OF E-LMV-OP throw ENDOF
      scatter-add OF E-LMV-OP throw ENDOF
   ;MATCH ;

: LMV-CHECK-DIMS ( -- )
   LMV-PN @ LMV-ARENA > if E-LMV-DIMS throw then
   LMV-NIN @ 0 ?do  LMV-INS i cells + @ LMV-REF-ELEMS LMV-ARENA > if E-LMV-DIMS throw then  loop ;

public
: LMV-ANALYZE ( n -- ) {: rid:n :}
   rid FP-REGION-MEMBERS drop                          \ validates FP-BUILD ran + rid range
   rid LMV-RID !
   rid LMV-FIND-NODE
   LMV-COLLECT-INS
   LMV-SET-DIMS
   LMV-CHECK-DIMS
   -1 LMV-BUILT? ! ;

\ ---- analysis accessors (lower-launch / lower-golden read these) ------------
: LMV-CK ( -- )  LMV-BUILT? @ 0= if E-LMV-STATE throw then ;
: LMV-NIN@ ( -- n )       LMV-CK LMV-NIN @ ;
: LMV-IN-REF@ ( n -- n ) {: i:n :}
   i 0 < i LMV-NIN @ >= or if E-LMV-REG throw then  LMV-INS i cells + @ ;
: LMV-IN-ELEMS@ ( n -- n ) {: i:n :}  i LMV-IN-REF@ LMV-REF-ELEMS ;   \ per-input upload size
: LMV-OUT-NODE@ ( -- n )  LMV-CK LMV-NODE @ ;
: LMV-ELEMS ( -- n )      LMV-CK LMV-PN @ ;
: LMV-PA@ ( -- n )        LMV-CK LMV-PA @ ;
: LMV-PB@ ( -- n )        LMV-CK LMV-PB @ ;
: LMV-RID@ ( -- n )       LMV-CK LMV-RID @ ;

private

\ ---- entry / regs / params (K buffers + out + a,b,n; cvta the pointers) ------
: LMV-KNAME ( -- )  s" REGION_" CG-S LMV-RID @ SB-U ;
: LMV-OUT-BASE ( -- n )  LMV-NIN @ 1+ ;                \ rd index of p_out (rd1..rdK buffers)
: LMV-ENTRY ( -- )
   SB-RESET
   s" .visible .entry " CG-S LMV-KNAME s" (" CG-S
   LMV-NIN @ 0 ?do  s" .param .u64 p_in" CG-S i SB-U s" , " CG-S  loop
   s" .param .u64 p_out, .param .u32 p_a, .param .u32 p_b, .param .u32 p_n)" CG-S
   CG-LINE ;
: LMV-OPEN ( -- )
   s" {" PTX-L
   s" .reg .pred %p<8>;" PTX-L
   s" .reg .f32 %f<16>;" PTX-L
   s" .reg .b32 %r<32>;" PTX-L
   s" .reg .b64 %rd<32>;" PTX-L ;
: LMV-PARAMS ( -- )
   LMV-NIN @ 0 ?do
      SB-RESET s" ld.param.u64 %rd" CG-S i 1+ SB-U s" , [p_in" CG-S i SB-U s" ];" CG-S CG-LINE
   loop
   SB-RESET s" ld.param.u64 %rd" CG-S LMV-OUT-BASE SB-U s" , [p_out];" CG-S CG-LINE
   s" ld.param.u32 %r1, [p_a];" PTX-L
   s" ld.param.u32 %r2, [p_b];" PTX-L
   s" ld.param.u32 %r3, [p_n];" PTX-L
   LMV-NIN @ 0 ?do
      SB-RESET s" cvta.to.global.u64 %rd" CG-S i 1+ SB-U s" , %rd" CG-S i 1+ SB-U s" ;" CG-S CG-LINE
   loop
   SB-RESET s" cvta.to.global.u64 %rd" CG-S LMV-OUT-BASE SB-U s" , %rd" CG-S LMV-OUT-BASE SB-U s" ;" CG-S CG-LINE ;

\ ---- flat element index + bounds (e = %r7, masked against p_n = %r3) ---------
: LMV-COORD ( -- )
   s" mov.u32 %r4, %ctaid.x;" PTX-L
   s" mov.u32 %r5, %ntid.x;" PTX-L
   s" mov.u32 %r6, %tid.x;" PTX-L
   s" mad.lo.u32 %r7, %r4, %r5, %r6;" PTX-L
   s" setp.ge.u32 %p1, %r7, %r3;" PTX-L
   s" @%p1 bra DONE;" PTX-L ;

\ store the loaded value (%f1) at out[e]  (out base = rd LMV-OUT-BASE)
: LMV-STORE ( -- )
   s" mul.wide.u32 %rd12, %r7, 4;" PTX-L
   SB-RESET s" add.u64 %rd13, %rd" CG-S LMV-OUT-BASE SB-U s" , %rd12;" CG-S CG-LINE
   s" st.global.f32 [%rd13], %f1;" PTX-L ;

\ ---- per-op bodies (source index remap; load -> %f1) ------------------------
: LMV-BODY-TRANSPOSE ( -- )                            \ i=e/R(%r1) j=e mod R src=j*C(%r2)+i
   s" div.u32 %r8, %r7, %r1;" PTX-L
   s" rem.u32 %r9, %r7, %r1;" PTX-L
   s" mad.lo.u32 %r10, %r9, %r2, %r8;" PTX-L
   s" mul.wide.u32 %rd10, %r10, 4;" PTX-L
   s" add.u64 %rd11, %rd1, %rd10;" PTX-L
   s" ld.global.f32 %f1, [%rd11];" PTX-L ;
: LMV-BODY-SLICE ( -- )                                \ src flat = e + p_a (r0*cols)
   s" add.u32 %r8, %r7, %r1;" PTX-L
   s" mul.wide.u32 %rd10, %r8, 4;" PTX-L
   s" add.u64 %rd11, %rd1, %rd10;" PTX-L
   s" ld.global.f32 %f1, [%rd11];" PTX-L ;
: LMV-BODY-CONCAT ( -- )                               \ e<split(%r1)?A[e]:B[e-split]
   s" setp.lt.u32 %p2, %r7, %r1;" PTX-L
   s" @%p2 bra FROMA;" PTX-L
   s" sub.u32 %r8, %r7, %r1;" PTX-L
   s" mul.wide.u32 %rd10, %r8, 4;" PTX-L
   s" add.u64 %rd11, %rd2, %rd10;" PTX-L
   s" ld.global.f32 %f1, [%rd11];" PTX-L
   s" bra AFTER;" PTX-L
   s" FROMA:" PTX-L
   s" mul.wide.u32 %rd10, %r7, 4;" PTX-L
   s" add.u64 %rd11, %rd1, %rd10;" PTX-L
   s" ld.global.f32 %f1, [%rd11];" PTX-L
   s" AFTER:" PTX-L ;
: LMV-BODY-GATHER ( -- )                               \ k=e/C(%r1) c=e mod C id=round(idx[k]) src=id*C+c
   s" div.u32 %r8, %r7, %r1;" PTX-L
   s" rem.u32 %r9, %r7, %r1;" PTX-L
   s" mul.wide.u32 %rd10, %r8, 4;" PTX-L
   s" add.u64 %rd11, %rd2, %rd10;" PTX-L
   s" ld.global.f32 %f2, [%rd11];" PTX-L
   s" add.f32 %f3, %f2, 0f3F000000;" PTX-L             \ + 0.5 (mirror EX-BUILD-IDX rounding)
   s" cvt.rzi.s32.f32 %r10, %f3;" PTX-L                \ truncate toward zero
   s" mad.lo.u32 %r11, %r10, %r1, %r9;" PTX-L
   s" mul.wide.u32 %rd12, %r11, 4;" PTX-L
   s" add.u64 %rd13, %rd1, %rd12;" PTX-L
   s" ld.global.f32 %f1, [%rd13];" PTX-L ;

: LMV-BODY ( -- )
   LMV-COORD
   LMV-NODE @ MIR-OP@ MATCH opkind
      transpose OF LMV-BODY-TRANSPOSE ENDOF
      slice     OF LMV-BODY-SLICE     ENDOF
      concat    OF LMV-BODY-CONCAT    ENDOF
      gather    OF LMV-BODY-GATHER    ENDOF
      add OF E-LMV-OP throw ENDOF  mul OF E-LMV-OP throw ENDOF
      scale OF E-LMV-OP throw ENDOF  bias OF E-LMV-OP throw ENDOF
      relu OF E-LMV-OP throw ENDOF  gelu OF E-LMV-OP throw ENDOF
      silu OF E-LMV-OP throw ENDOF  layernorm OF E-LMV-OP throw ENDOF
      rmsnorm OF E-LMV-OP throw ENDOF  softmax-row OF E-LMV-OP throw ENDOF
      matmul OF E-LMV-OP throw ENDOF  linear OF E-LMV-OP throw ENDOF
      residual-add OF E-LMV-OP throw ENDOF  cast OF E-LMV-OP throw ENDOF
      rope OF E-LMV-OP throw ENDOF  reshape OF E-LMV-OP throw ENDOF
      relu-bwd OF E-LMV-OP throw ENDOF  gelu-bwd OF E-LMV-OP throw ENDOF
      silu-bwd OF E-LMV-OP throw ENDOF  layernorm-bwd OF E-LMV-OP throw ENDOF
      rmsnorm-bwd OF E-LMV-OP throw ENDOF  softmax-row-bwd OF E-LMV-OP throw ENDOF
      rope-bwd OF E-LMV-OP throw ENDOF  rowsum-bwd OF E-LMV-OP throw ENDOF
      fullsum-dot-bwd OF E-LMV-OP throw ENDOF  pad-scatter OF E-LMV-OP throw ENDOF
      scatter-add OF E-LMV-OP throw ENDOF
   ;MATCH
   LMV-STORE ;

public
\ LMV-EMIT prints the region's copy-kernel PTX module to the current PTX sink. Analysis
\ first, so a rejected region emits nothing.
: LMV-EMIT ( n -- )
   LMV-ANALYZE
   PTX-MODULE{
      LMV-ENTRY  LMV-OPEN  LMV-PARAMS
      LMV-BODY
      s" DONE:" PTX-L
      s" ret;" PTX-L
      s" }" PTX-L
   }PTX-MODULE ;

end-package
