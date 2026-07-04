\ maki/lower-mm.f - lower ONE matmul/linear fusion region to a tiled-GEMM PTX kernel.
\
\ CAD-PLAN sections 5/8/11, device leg slice 3. The contraction dual of maki/lower-ew.f
\ and maki/lower-red.f. Given the model-IR node table (maki/model-ir.f) and its fusion
\ plan (maki/fusion-plan.f), LMM-EMIT lowers a region whose class mix is MATMUL (with an
\ optional TRAILING elementwise EPILOGUE fused after the single contraction - exactly what
\ FP-BUILD produces: bare MATMUL/LINEAR are classmix MATMUL, MATMUL->RELU and LINEAR->GELU
\ fuse the activation epilogue into the same region) to the PTX SOURCE of a 2D-tiled GEMM
\ over C[M,N] = A[M,K] . B[K,N]:
\   grid  = (ceil(N/16), ceil(M/16)) 2D ; block = 16x16 = 256 threads ; one output
\           element C[row,col] per thread, masked against row<M (%r1) and col<N (%r2).
\   K-loop = the runtime contraction: acc(f32) += A[row,kk]*B[kk,col] over kk in 0..K
\           (fma.rn.f32). LINEAR folds a 1xN bias add after the K-loop; the epilogue EW
\           chain (relu/gelu/silu) then applies to the accumulator BEFORE the store.
\
\ TWO kernels, dispatched on shape (LMM-BLOCKED?):
\   BLOCKED (perf) - a register-blocked 64x64 SGEMM when M,N are multiples of 64 and K a
\     multiple of 16. One 64x64 output tile per block; block = 16x16 = 256 threads; each
\     thread owns a 4x4 register accumulator micro-tile (%f10..%f25). K swept in BK=32
\     tiles (the gemm-tf32-v1 family floor): the 256 threads cooperatively stage
\     As[64][32]+Bs[32][64] to .shared, bar.sync, then each thread does its 4x4
\     outer-product accumulate (register blocking is the win - 16 FMAs reuse the 4 A
\     scalar + 1 B ld.shared.v4 loads). REUSES the cg-matmul.f vocabulary verbatim
\     (MM-THREAD-SETUP / MM-ACC-ZERO-EMIT / MM-SMEM-BASES / MM-STAGE / MM-KSTEP); the ONLY
\     re-expression is the fused epilogue write (LMM-BLK-WRITE), which folds bias[col] and the
\     activation chain into each of the 16 micro-tile stores under lower-mm's fusion ABI (out
\     at %rd<OUT-BASE>, bias at %rd3), where cg-matmul's bare MM-WRITE stored a single C.
\   NAIVE (fallback) - the correctness-first one-element-per-thread 16x16 tile for any other
\     (small / unpadded) shape, bounds-masked so tiny golden shapes match the host reference.
\ The block SHAPE is 16x16 = 256 threads for BOTH; only the OUTPUT-tile granularity differs
\ (LMM-OUT-TILE@ = 64 blocked / 16 naive), which is the launch-grid divisor lower-launch reads.
\
\ Schedule-family mapping (maki/schedule.f gemm-tf32-v1 = bm{64,128} bn{64,128} bk{32,64}
\ warps{4,8} stages{1,2}): the blocked tile is bm=64 bn=64 warps=8 stages=1 - a candidate of
\ that family (GEMM-DECODE index 2 on those four axes). bk=32 is the family floor (8.1 step 2A,
\ with a vectorized ld.shared.v4 B load); cp.async multi-stage (family `stages`, step 2B) + MMA
\ are the next 8.1 steps that the cad-6 autotuner will search.
\
\ Emit-mode discipline mirrors lower-red.f: the fixed GEMM prologue/K-loop/store use
\ explicit registers (%f1 = accumulator, %r/%rd address math); the epilogue reuses the
\ cg-activation.f EMIT-RELU/EMIT-GELU/EMIT-SILU emitters (each returns a fresh %f via
\ CG-NEXT-F) op-for-op with the host references, so the device f32 matches F64>F32(host)
\ under the section 11 matmul tolerance (maki/lower-golden.f). Kernel ABI (REGION_<rid>):
\ one `.param .u64 p_in<i>` per contraction operand (A, B, [bias]), then `.param .u64
\ p_out`, then `.param .u32 p_m, p_n, p_k`.
\
\ Fail closed BEFORE any PTX is emitted (a rejected region emits nothing): a region whose
\ class mix is not MATMUL(+EW) (E-LMM-NOTMM), a member op that is neither the contraction
\ nor a v1 unary EW epilogue (E-LMM-OP), a region without exactly one contraction node
\ (E-LMM-MULTIMM, v1 cap: one contraction per region), an EW op that feeds INTO the
\ contraction i.e. a fused PROLOGUE (E-LMM-PROLOGUE, unsupported in v1 - the epilogue-only
\ kernel cannot pre-transform the contraction operands), a region without exactly one
\ materialized output (E-LMM-MULTIOUT), an operand shape that disagrees with the M/N/K the
\ contraction node declares (E-LMM-DIMS), a buffer element count above the launch arena
\ (E-LMM-DIMS, v1 cap 4096 f32), and K above the accumulation cap (E-LMM-KCAP, v1 K<=256)
\ are named throws. maki -> habu only; lower-mm owns -5193..-5200. Load after the cg emit
\ stack (see requires).

require lib/errors.f
require lib/string.f
require lib/float.f
require lib/fmt.f
require src/arch/ptx/emit.f
require lib/ptx/header.f
require lib/ptx/cg.f
require lib/ptx/cg-collective.f
require lib/ptx/cg-activation.f
require lib/ptx/cg-matmul.f
require maki/op-kind.f
require maki/op-registry.f
require maki/model-ir.f
require maki/fusion-plan.f
require maki/move-view.f

-5193 constant E-LMM-NOTMM     \ region class mix is not a supported matmul region
-5194 constant E-LMM-OP        \ a region op is neither the contraction nor a v1 unary EW epilogue
-5195 constant E-LMM-MULTIMM   \ region does not have exactly one contraction node (v1 cap)
-5196 constant E-LMM-PROLOGUE  \ an EW op feeds INTO the contraction (prologue fusion unsupported v1)
-5197 constant E-LMM-MULTIOUT  \ region does not have exactly one materialized output
-5198 constant E-LMM-DIMS      \ operand shape disagrees with M/N/K, or a buffer exceeds the arena cap
-5199 constant E-LMM-KCAP      \ contraction inner dim K exceeds the v1 accumulation cap (K<=256)
-5200 constant E-LMM-REG       \ node / input register-map index out of range

package MAKI
private

4    constant LMM-MAX-IN       \ contraction arity cap (linear=3, matmul=2, both <=4)
128  constant LMM-NCAP         \ node register-map size (mirrors model-ir MIR-CAP)
16   constant LMM-TILE         \ output tile edge (16x16 block = 256 threads)
256  constant LMM-KCAP         \ v1 accumulation cap (documented): K <= 256
4096 constant LMM-ARENA        \ v1 buffer element cap (mirrors lower-launch LLA-NCAP: 16 KB f32)

create LMM-INS      LMM-MAX-IN cells allot   \ ordered contraction operand refs (A, B, [bias])
create LMM-NODE-REG LMM-NCAP cells allot     \ result %f register per region-member node
variable LMM-NIN                              \ contraction operand count (2 or 3)
variable LMM-MMNODE                           \ the single contraction node (matmul / linear)
variable LMM-OUTNODE                          \ the single materialized region-output node
variable LMM-RID                              \ region id being lowered
variable LMM-M  variable LMM-N  variable LMM-K \ C = A(MxK) . B(KxN)
variable LMM-BLK                               \ 1 = shape fits the register-blocked 64x64 tile

\ ---- region membership + class ---------------------------------------------
: LMM-IN-REGION? ( n n -- bool )  swap FP-RID@ = ;      \ node rid -- in-region?

\ class mix must contain the MATMUL bit and nothing outside {EW, MATMUL, MOVEMENT};
\ a dissolved free-movement operand (maki/move-view.f) folds into the K-loop's A base offset.
: LMM-CLASS-OK? ( n -- bool ) {: rid:n :}
   rid FP-REGION-CLASSMIX {: mix:n :}
   mix 1 CLASS-MATMUL lshift and 0= if false exit then
   mix  1 CLASS-EW lshift  1 CLASS-MATMUL lshift or  1 CLASS-MOVEMENT lshift or
      invert and 0= ;

\ ---- supported op sets -----------------------------------------------------
: LMM-MM-OP?  ( n -- bool ) {: op:n :}  op OP-MATMUL = op OP-LINEAR = or ;      \ the contraction
: LMM-EPI-OP? ( n -- bool ) {: op:n :}  op OP-RELU = op OP-GELU = or op OP-SILU = or ; \ v1 unary EW epilogue

: LMM-MM-COUNT ( n -- n ) {: rid:n :}          \ contraction nodes in the region
   0 MIR-N@ 0 ?do  i rid LMM-IN-REGION? i MIR-OP@ LMM-MM-OP? and if 1+ then  loop ;

\ compute members must be the contraction or a v1 unary EW epilogue; movement members must
\ be v1-foldable dissolved movements (MVW-CHECK fails closed on staged / mat / non-slot source).
: LMM-CHECK-OPS ( n -- ) {: rid:n :}
   MIR-N@ 0 ?do
      i rid LMM-IN-REGION? if
         i MIR-MOVE? if i MVW-CHECK
         else
            i MIR-OP@ {: op:n :}
            op LMM-MM-OP? op LMM-EPI-OP? or 0= if E-LMM-OP throw then
         then
      then
   loop
   rid LMM-MM-COUNT 1 <> if E-LMM-MULTIMM throw then ;

: LMM-FIND-MM ( n -- ) {: rid:n :}             \ the sole contraction node (count checked already)
   -1 LMM-MMNODE !
   MIR-N@ 0 ?do  i rid LMM-IN-REGION? i MIR-OP@ LMM-MM-OP? and if i LMM-MMNODE ! then  loop ;

\ ---- prologue guard: the contraction's operands must all be external ---------
\ An EW region-member whose output is a contraction operand is a fused PROLOGUE; the
\ epilogue-only v1 kernel cannot pre-transform A/B/bias, so such a region fails closed.
\ A dissolved FREE movement operand folds into the A-base K-loop offset (maki/move-view.f);
\ a NON-movement interior producer is a compute prologue the epilogue-only v1 kernel cannot run.
: LMM-CHECK-PROLOGUE ( -- )
   LMM-MMNODE @ {: mm:n :}  LMM-RID @ {: rid:n :}
   mm MIR-IN-COUNT@ 0 ?do
      mm i MIR-IN@ {: ref:n :}
      ref MIR-REF-INPUT? 0= if
         ref MVW-DISSOLVED? if ref MVW-CHECK            \ folded free-movement operand: allowed
         else ref FP-RID@ rid = if E-LMM-PROLOGUE throw then then
      then
   loop ;

\ ---- contraction operands become the kernel inputs (A, B, [bias]) -----------
: LMM-COLLECT-INS ( -- )
   LMM-MMNODE @ {: mm:n :}
   mm MIR-IN-COUNT@ {: c:n :}
   c LMM-MAX-IN > if E-LMM-REG throw then
   c LMM-NIN !
   c 0 ?do  mm i MIR-IN@  LMM-INS i cells + !  loop ;

\ ---- single materialized output --------------------------------------------
: LMM-MAT-COUNT ( n -- n ) {: rid:n :}
   0 MIR-N@ 0 ?do  i rid LMM-IN-REGION? i MIR-MAT@ and if 1+ then  loop ;
: LMM-FIND-OUT ( n -- ) {: rid:n :}
   rid LMM-MAT-COUNT 1 <> if E-LMM-MULTIOUT throw then
   -1 LMM-OUTNODE !
   MIR-N@ 0 ?do  i rid LMM-IN-REGION? i MIR-MAT@ and if i LMM-OUTNODE ! then  loop ;

\ ---- operand-ref shape (input slot or interior producer) --------------------
: LMM-REF-ROWS ( n -- n ) {: ref:n :}
   ref MIR-REF-INPUT? if ref MIR-REF-SLOT MIR-SLOT-ROWS@ else ref MIR-ROWS@ then ;
: LMM-REF-COLS ( n -- n ) {: ref:n :}
   ref MIR-REF-INPUT? if ref MIR-REF-SLOT MIR-SLOT-COLS@ else ref MIR-COLS@ then ;

\ ---- dims (M/N from the contraction output; K from A's cols) ----------------
: LMM-SET-DIMS ( -- )
   LMM-MMNODE @ {: mm:n :}
   mm MIR-ROWS@ LMM-M !
   mm MIR-COLS@ LMM-N !
   LMM-INS 0 cells + @ LMM-REF-COLS LMM-K ! ;

\ shapes must be self-consistent (defense-in-depth: A MxK, B KxN, bias 1xN) ----
: LMM-CHECK-SHAPES ( -- )
   LMM-M @ {: M:n :}  LMM-N @ {: N:n :}  LMM-K @ {: K:n :}
   LMM-INS 0 cells + @ {: a:n :}
   a LMM-REF-ROWS M <>  a LMM-REF-COLS K <>  or if E-LMM-DIMS throw then
   LMM-INS 1 cells + @ {: b:n :}
   b LMM-REF-ROWS K <>  b LMM-REF-COLS N <>  or if E-LMM-DIMS throw then
   LMM-NIN @ 3 = if
      LMM-INS 2 cells + @ {: bias:n :}
      bias LMM-REF-ROWS 1 <>  bias LMM-REF-COLS N <>  or if E-LMM-DIMS throw then
   then ;

: LMM-CHECK-DIMS ( -- )
   LMM-K @ LMM-KCAP > if E-LMM-KCAP throw then
   LMM-M @ LMM-N @ * LMM-ARENA > if E-LMM-DIMS throw then     \ output C(MxN)
   LMM-M @ LMM-K @ * LMM-ARENA > if E-LMM-DIMS throw then     \ input A(MxK)
   LMM-K @ LMM-N @ * LMM-ARENA > if E-LMM-DIMS throw then ;   \ input B(KxN)

\ shape fits the register-blocked tile iff M,N are multiples of the 64x64 output tile and K a
\ multiple of the BK=32 K-step (cg-matmul.f MM-BM/MM-BN/MM-BK); otherwise the naive tile runs.
: LMM-SET-BLK ( -- )
   LMM-M @ MM-BM mod
   LMM-N @ MM-BN mod or
   LMM-K @ MM-BK mod or                            \ 0 iff all three are exact multiples
   0= if 1 else 0 then LMM-BLK ! ;

public
: LMM-ANALYZE ( n -- ) {: rid:n :}
   rid FP-REGION-MEMBERS drop                    \ validates FP-BUILD ran + rid range
   rid LMM-RID !
   rid LMM-CLASS-OK? 0= if E-LMM-NOTMM throw then
   rid LMM-CHECK-OPS
   rid LMM-FIND-MM
   LMM-CHECK-PROLOGUE
   LMM-COLLECT-INS
   rid LMM-FIND-OUT
   LMM-SET-DIMS
   LMM-CHECK-SHAPES
   LMM-CHECK-DIMS
   LMM-SET-BLK ;

\ ---- analysis accessors (lower-launch / lower-golden read these) ------------
: LMM-NIN@ ( -- n )       LMM-NIN @ ;
: LMM-IN-REF@ ( n -- n ) {: i:n :}
   i 0 < i LMM-NIN @ >= or if E-LMM-REG throw then  LMM-INS i cells + @ ;
: LMM-IN-ELEMS@ ( n -- n ) {: i:n :}             \ input i's element count (A/B/bias differ)
   i LMM-IN-REF@ {: ref:n :}  ref LMM-REF-ROWS  ref LMM-REF-COLS * ;
: LMM-OUT-NODE@ ( -- n )  LMM-OUTNODE @ ;
: LMM-M@ ( -- n )         LMM-M @ ;
: LMM-N@ ( -- n )         LMM-N @ ;
: LMM-K@ ( -- n )         LMM-K @ ;
: LMM-ELEMS ( -- n )      LMM-M @ LMM-N @ * ;
: LMM-RID@ ( -- n )       LMM-RID @ ;
: LMM-BLOCKED? ( -- bool ) LMM-BLK @ 0= 0= ;    \ register-blocked tile chosen for this shape?
: LMM-OUT-TILE@ ( -- n )  LMM-BLK @ if MM-BM else LMM-TILE then ;  \ launch-grid output-tile edge

private

\ ---- register map (member node result reg; epilogue operand resolution) -----
: LMM-NR@ ( n -- n ) {: nd:n :}
   nd 0 < nd LMM-NCAP >= or if E-LMM-REG throw then  LMM-NODE-REG nd cells + @ ;
: LMM-NR! ( n n -- ) {: r:n nd:n :}
   nd 0 < nd LMM-NCAP >= or if E-LMM-REG throw then  r LMM-NODE-REG nd cells + ! ;

\ a unary epilogue node reads operand-0 (the contraction or a prior epilogue reg)
: LMM-EPI-OPREG ( n -- n ) {: nd:n :}  nd 0 MIR-IN@ LMM-NR@ ;
: LMM-EPI-NODE ( n -- ) {: nd:n :}
   nd MIR-OP@ case
      OP-RELU of nd LMM-EPI-OPREG EMIT-RELU endof
      OP-GELU of nd LMM-EPI-OPREG EMIT-GELU endof
      OP-SILU of nd LMM-EPI-OPREG EMIT-SILU endof
      E-LMM-OP throw
   endcase
   nd LMM-NR! ;
: LMM-EPI-CHAIN ( -- )                           \ epilogue nodes in topo order (skip folded movement)
   MIR-N@ 0 ?do
      i LMM-RID @ LMM-IN-REGION?  i LMM-MMNODE @ <>  and  i MIR-MOVE? 0= and if i LMM-EPI-NODE then
   loop ;

\ fold each dissolved free-movement contraction operand into a base-pointer offset (reshape 0 /
\ slice r0*K*4): the operand's global pointer is advanced after LMM-PARAMS cvta's it, so the
\ K-loop reads the movement's source window with no other change (maki/move-view.f).
: LMM-APPLY-VIEWS ( -- )
   LMM-NIN @ 0 ?do
      LMM-INS i cells + @ MVW-RESOLVE-OFF {: off:n :}
      off 0 > if
         SB-RESET s" add.u64 %rd" CG-S i 1+ SB-U s" , %rd" CG-S i 1+ SB-U s" , " CG-S off SB-U s" ;" CG-S CG-LINE
      then
   loop ;

\ ---- entry / regs / params (K inputs + output + M,N,K) ----------------------
: LMM-KNAME ( -- )  s" REGION_" CG-S LMM-RID @ SB-U ;
: LMM-OUT-BASE ( -- n )  LMM-NIN @ 1+ ;          \ rd index of p_out (rd1..rdK inputs, rd K+1 out)
: LMM-ENTRY ( -- )
   SB-RESET
   s" .visible .entry " CG-S LMM-KNAME s" (" CG-S
   LMM-NIN @ 0 ?do  s" .param .u64 p_in" CG-S i SB-U s" , " CG-S  loop
   s" .param .u64 p_out, .param .u32 p_m, .param .u32 p_n, .param .u32 p_k)" CG-S
   CG-LINE ;
: LMM-OPEN ( -- )
   s" {" PTX-L
   s" .reg .pred %p<16>;" PTX-L
   s" .reg .f32 %f<256>;" PTX-L
   s" .reg .b32 %r<64>;" PTX-L
   s" .reg .b64 %rd<64>;" PTX-L ;
: LMM-PARAMS ( -- )
   LMM-NIN @ 0 ?do
      SB-RESET s" ld.param.u64 %rd" CG-S i 1+ SB-U s" , [p_in" CG-S i SB-U s" ];" CG-S CG-LINE
   loop
   SB-RESET s" ld.param.u64 %rd" CG-S LMM-OUT-BASE SB-U s" , [p_out];" CG-S CG-LINE
   s" ld.param.u32 %r1, [p_m];" PTX-L
   s" ld.param.u32 %r2, [p_n];" PTX-L
   s" ld.param.u32 %r3, [p_k];" PTX-L
   LMM-NIN @ 0 ?do
      SB-RESET s" cvta.to.global.u64 %rd" CG-S i 1+ SB-U s" , %rd" CG-S i 1+ SB-U s" ;" CG-S CG-LINE
   loop
   SB-RESET s" cvta.to.global.u64 %rd" CG-S LMM-OUT-BASE SB-U s" , %rd" CG-S LMM-OUT-BASE SB-U s" ;" CG-S CG-LINE ;

: LMM-RESET-REGS ( -- )                          \ counters past the fixed GEMM regs (acc=%f1)
   4 CG-NF !  20 CG-NRD !  20 CG-NR !  8 CG-NP !  0 CG-NL ! ;

\ ---- body: 2D coords + bounds, K-loop, [bias], epilogue chain, masked store --
: LMM-COORDS ( -- )
   s" mov.u32 %r4, %ctaid.y;" PTX-L
   s" mov.u32 %r5, %ntid.y;" PTX-L
   s" mov.u32 %r6, %tid.y;" PTX-L
   s" mad.lo.u32 %r7, %r4, %r5, %r6;" PTX-L        \ row = ctaid.y*ntid.y + tid.y
   s" mov.u32 %r8, %ctaid.x;" PTX-L
   s" mov.u32 %r9, %ntid.x;" PTX-L
   s" mov.u32 %r10, %tid.x;" PTX-L
   s" mad.lo.u32 %r11, %r8, %r9, %r10;" PTX-L      \ col = ctaid.x*ntid.x + tid.x
   s" setp.ge.u32 %p1, %r7, %r1;" PTX-L
   s" @%p1 bra DONE;" PTX-L
   s" setp.ge.u32 %p2, %r11, %r2;" PTX-L
   s" @%p2 bra DONE;" PTX-L ;

: LMM-KLOOP ( -- )
   s" mov.f32 %f1, 0f00000000;" PTX-L              \ acc = 0 (f32)
   s" mov.u32 %r12, 0;" PTX-L                       \ kk = 0
   s" $KLOOP:" PTX-L
   s" setp.ge.u32 %p3, %r12, %r3;" PTX-L
   s" @%p3 bra $KEND;" PTX-L
   s" mad.lo.u32 %r13, %r7, %r3, %r12;" PTX-L       \ A index = row*K + kk
   s" mul.wide.u32 %rd10, %r13, 4;" PTX-L
   s" add.u64 %rd11, %rd1, %rd10;" PTX-L
   s" ld.global.f32 %f2, [%rd11];" PTX-L
   s" mad.lo.u32 %r14, %r12, %r2, %r11;" PTX-L      \ B index = kk*N + col
   s" mul.wide.u32 %rd12, %r14, 4;" PTX-L
   s" add.u64 %rd13, %rd2, %rd12;" PTX-L
   s" ld.global.f32 %f3, [%rd13];" PTX-L
   s" fma.rn.f32 %f1, %f2, %f3, %f1;" PTX-L
   s" add.u32 %r12, %r12, 1;" PTX-L
   s" bra $KLOOP;" PTX-L
   s" $KEND:" PTX-L ;

: LMM-BIAS ( -- )                                  \ acc += bias[col]  (LINEAR; bias base = %rd3)
   s" mul.wide.u32 %rd10, %r11, 4;" PTX-L
   s" add.u64 %rd11, %rd3, %rd10;" PTX-L
   s" ld.global.f32 %f2, [%rd11];" PTX-L
   s" add.rn.f32 %f1, %f1, %f2;" PTX-L ;

: LMM-STORE ( -- )                                 \ C[row*N + col] = the output-node register
   LMM-OUTNODE @ LMM-NR@ {: outf:n :}
   s" mad.lo.u32 %r13, %r7, %r2, %r11;" PTX-L
   s" mul.wide.u32 %rd10, %r13, 4;" PTX-L
   SB-RESET s" add.u64 %rd11, %rd" CG-S LMM-OUT-BASE SB-U s" , %rd10;" CG-S CG-LINE
   SB-RESET s" st.global.f32 [%rd11], %f" CG-S outf SB-U s" ;" CG-S CG-LINE ;

: LMM-BODY ( -- )
   LMM-COORDS
   LMM-KLOOP
   LMM-MMNODE @ MIR-OP@ OP-LINEAR = if LMM-BIAS then
   1 LMM-MMNODE @ LMM-NR!                          \ contraction result = %f1 (after any bias)
   LMM-RESET-REGS
   LMM-EPI-CHAIN
   LMM-STORE ;

\ ---- blocked (register-tiled 64x64) body: reuse the cg-matmul.f staging/compute vocabulary --
\ MM-THREAD-SETUP (r4..r11), MM-ACC-ZERO-EMIT (%f10..%f25 = 0), MM-SMEM-BASES (r12/r13), and
\ MM-STAGE / MM-KSTEP (%f26..%f33 loads + 4x4 FMAs) are emitted verbatim; A=%rd1, B=%rd2 match
\ this ABI. Only the K-loop scaffold and the epilogue write are re-expressed for fusion here.
: LMM-BLK-SHARED ( -- )                            \ .shared As[64][32] + Bs[32][64] (SH symbol, v4-aligned)
   SB-RESET s" .shared .align 16 .b8 SH[" CG-S MM-ASB MM-BSB + SB-U s" ];" CG-S CG-LINE ;

\ MM-K-LOOP body with a ( -- ) effect (cg-matmul's is typed on mmctx/mmacc, which this
\ non-KERNEL lowering does not thread); the staged As/Bs + 4x4 accumulate are MM-STAGE/MM-KSTEP.
: LMM-BLK-KLOOP ( -- )
   s" mov.u32 %r14, 0;" PTX-L  s" $KLOOP:" PTX-L
   s" setp.ge.u32 %p1, %r14, %r3;" PTX-L  s" @%p1 bra $KEND;" PTX-L
   MM-NSTG 0 ?do  i MM-STAGE  loop
   s" bar.sync 0;" PTX-L
   MM-BK 0 ?do  i MM-KSTEP  loop
   s" bar.sync 0;" PTX-L  s" add.u32 %r14, %r14, 32;" PTX-L  s" bra $KLOOP;" PTX-L  s" $KEND:" PTX-L ;

: LMM-BLK-CBASE ( -- )                             \ %r40 = rowBase + ty*4 ; %r41 = colBase + tx*4
   s" shl.b32 %r40, %r5, 2;" PTX-L  s" add.u32 %r40, %r9, %r40;" PTX-L
   s" shl.b32 %r41, %r4, 2;" PTX-L  s" add.u32 %r41, %r10, %r41;" PTX-L ;

\ one micro-tile element (tile-row j, tile-col i): acc %f(10+j*4+i) -> fold bias[cCol] (LINEAR)
\ -> activation chain -> store to out[cRow*N + cCol]. The epilogue reuses LMM-EPI-CHAIN by
\ pointing the contraction node's register at this element and reading the output node's result.
: LMM-BLK-ELEM ( n n -- ) {: j:n i:n :}
   SB-RESET s" add.u32 %r42, %r40, " CG-S j SB-U s" ;" CG-S CG-LINE       \ cRow = cRow0 + j
   SB-RESET s" add.u32 %r43, %r41, " CG-S i SB-U s" ;" CG-S CG-LINE       \ cCol = cCol0 + i
   s" mad.lo.u32 %r44, %r42, %r2, %r43;" PTX-L                            \ gidx = cRow*N + cCol
   10 j 4 * + i + {: accf:n :}                                           \ accumulator %f(10+j*4+i)
   LMM-MMNODE @ MIR-OP@ OP-LINEAR = if                                   \ bias fusion: acc += bias[cCol]
      s" mul.wide.u32 %rd10, %r43, 4;" PTX-L
      s" add.u64 %rd11, %rd3, %rd10;" PTX-L
      s" ld.global.f32 %f4, [%rd11];" PTX-L
      SB-RESET s" add.rn.f32 %f5, %f" CG-S accf SB-U s" , %f4;" CG-S CG-LINE
      5
   else accf then {: srcf:n :}
   40 CG-NF !                                                            \ epilogue temps above tile/staging regs
   srcf LMM-MMNODE @ LMM-NR!
   LMM-EPI-CHAIN
   LMM-OUTNODE @ LMM-NR@ {: outf:n :}
   s" mul.wide.u32 %rd10, %r44, 4;" PTX-L
   SB-RESET s" add.u64 %rd11, %rd" CG-S LMM-OUT-BASE SB-U s" , %rd10;" CG-S CG-LINE
   SB-RESET s" st.global.f32 [%rd11], %f" CG-S outf SB-U s" ;" CG-S CG-LINE ;

: LMM-BLK-WRITE ( -- )                             \ fused epilogue over the thread's 4x4 micro-tile
   LMM-BLK-CBASE
   MM-TM 0 ?do  MM-TM 0 ?do  j i LMM-BLK-ELEM  loop  loop ;

: LMM-BLK-BODY ( -- )
   MM-THREAD-SETUP
   MM-ACC-ZERO-EMIT
   MM-SMEM-BASES
   LMM-BLK-KLOOP
   LMM-BLK-WRITE ;

public
\ LMM-EMIT prints the region's PTX module to the current PTX sink (stdout, or the
\ in-process capture buffer). Analysis first, so a rejected region emits nothing.
: LMM-EMIT ( n -- )
   LMM-ANALYZE
   PTX-MODULE{
      LMM-ENTRY  LMM-OPEN
      LMM-BLK @ if LMM-BLK-SHARED then
      LMM-PARAMS  LMM-APPLY-VIEWS
      LMM-BLK @ if LMM-BLK-BODY else LMM-BODY then
      s" DONE:" PTX-L
      s" ret;" PTX-L
      s" }" PTX-L
   }PTX-MODULE ;

end-package
