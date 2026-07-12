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
\     (MM-THREAD-SETUP / MM-ACC-ZERO-EMIT / MM-PIPE-KLOOP / MM-KSTEP); the ONLY
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
\ kernel cannot pre-transform the contraction operands), a region whose materialized-output
\ count is not exactly one (E-LMM-MULTIOUT - a defense-in-depth PLANNER-INVARIANT guard, not a
\ v1 cap: the planner always plans exactly one materialized output per region, proven in
\ maki/fusion-mout-test.f), an operand shape that disagrees with the M/N/K the
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
require lib/ptx/cg-mma.f
require maki/op-kind.f
require maki/op-registry.f
require maki/precision.f
require maki/model-ir.f
require maki/fusion-plan.f
require maki/move-view.f

-5193 constant E-LMM-NOTMM     \ region class mix is not a supported matmul region
-5194 constant E-LMM-OP        \ a region op is neither the contraction nor a v1 unary EW epilogue
-5195 constant E-LMM-MULTIMM   \ region does not have exactly one contraction node (v1 cap)
-5196 constant E-LMM-PROLOGUE  \ an EW op feeds INTO the contraction (prologue fusion unsupported v1)
-5197 constant E-LMM-MULTIOUT  \ region's materialized-output count != 1 (planner-invariant guard, not a cap)
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
1 LAYOUT-BUFFER LMM-MMNODE CAD-KIND:node-id   \ the single contraction node (matmul / linear; typed 1-slot cell)
1 LAYOUT-BUFFER LMM-OUTNODE CAD-KIND:node-id  \ the single materialized region-output node (typed 1-slot cell)
1 LAYOUT-BUFFER LMM-RID CAD-KIND:region       \ region id being lowered (typed 1-slot cell)
variable LMM-M  variable LMM-N  variable LMM-K \ C = A(MxK) . B(KxN)
variable LMM-BLK                               \ 1 = shape fits the register-blocked 64x64 tile

: LMM-REF! ( MIR:operand-ref n -- )  cells LMM-INS + ! ;
: LMM-REF@ ( n -- MIR:operand-ref )  cells LMM-INS + @ ;
: LMM-MMNODE! ( CAD-KIND:node-id -- )  0 LMM-MMNODE ! ;
: LMM-MMNODE@ ( -- CAD-KIND:node-id )  0 LMM-MMNODE @ ;
: LMM-OUTNODE! ( CAD-KIND:node-id -- )  0 LMM-OUTNODE ! ;
: LMM-OUTNODE@ ( -- CAD-KIND:node-id )  0 LMM-OUTNODE @ ;

\ ---- region membership + class ---------------------------------------------
: LMM-IN-REGION? ( CAD-KIND:node-id CAD-KIND:region -- bool )  swap FP-RID@ FP-RGN= ;

\ class mix must contain the MATMUL bit and nothing outside {EW, MATMUL, MOVEMENT};
\ a dissolved free-movement operand (maki/move-view.f) folds into the K-loop's A base offset.
: LMM-CLASS-OK? ( CAD-KIND:region -- bool ) {: rid:CAD-KIND:region :}
   rid FP-REGION-CLASSMIX {: mix:n :}
   mix 1 CLASS-MATMUL lshift and 0= if false exit then
   mix  1 CLASS-EW lshift  1 CLASS-MATMUL lshift or  1 CLASS-MOVEMENT lshift or
      invert and 0= ;

\ ---- supported op sets -----------------------------------------------------
: LMM-MM-OP?  ( opkind -- bool )                 \ the contraction
   MATCH opkind
      matmul OF true ENDOF  linear OF true ENDOF
      add OF false ENDOF  mul OF false ENDOF  scale OF false ENDOF  bias OF false ENDOF
      relu OF false ENDOF  gelu OF false ENDOF  silu OF false ENDOF  layernorm OF false ENDOF
      rmsnorm OF false ENDOF  softmax-row OF false ENDOF  residual-add OF false ENDOF
      cast OF false ENDOF  rope OF false ENDOF  reshape OF false ENDOF  transpose OF false ENDOF
      slice OF false ENDOF  concat OF false ENDOF  gather OF false ENDOF  relu-bwd OF false ENDOF
      gelu-bwd OF false ENDOF  silu-bwd OF false ENDOF  layernorm-bwd OF false ENDOF
      rmsnorm-bwd OF false ENDOF  softmax-row-bwd OF false ENDOF  rope-bwd OF false ENDOF
      rowsum-bwd OF false ENDOF  fullsum-dot-bwd OF false ENDOF  pad-scatter OF false ENDOF
      scatter-add OF false ENDOF  gelu-bwd2 OF false ENDOF
   ;MATCH ;
: LMM-EPI-OP? ( opkind -- bool )                 \ v1 unary EW epilogue
   MATCH opkind
      relu OF true ENDOF  gelu OF true ENDOF  silu OF true ENDOF
      add OF false ENDOF  mul OF false ENDOF  scale OF false ENDOF  bias OF false ENDOF
      layernorm OF false ENDOF  rmsnorm OF false ENDOF  softmax-row OF false ENDOF
      matmul OF false ENDOF  linear OF false ENDOF  residual-add OF false ENDOF
      cast OF false ENDOF  rope OF false ENDOF  reshape OF false ENDOF  transpose OF false ENDOF
      slice OF false ENDOF  concat OF false ENDOF  gather OF false ENDOF  relu-bwd OF false ENDOF
      gelu-bwd OF false ENDOF  silu-bwd OF false ENDOF  layernorm-bwd OF false ENDOF
      rmsnorm-bwd OF false ENDOF  softmax-row-bwd OF false ENDOF  rope-bwd OF false ENDOF
      rowsum-bwd OF false ENDOF  fullsum-dot-bwd OF false ENDOF  pad-scatter OF false ENDOF
      scatter-add OF false ENDOF  gelu-bwd2 OF false ENDOF
   ;MATCH ;

: LMM-MM-COUNT ( CAD-KIND:region -- n ) {: rid:CAD-KIND:region :}   \ contraction nodes in the region
   0 MIR-N@ 0 ?do
      i MIR-NODE-ID {: node:CAD-KIND:node-id :}
      node rid LMM-IN-REGION? node MIR-OP@ LMM-MM-OP? and if 1+ then
   loop ;

\ compute members must be the contraction or a v1 unary EW epilogue; movement members must
\ be v1-foldable dissolved movements (MVW-CHECK fails closed on staged / mat / non-slot source).
: LMM-CHECK-OPS ( CAD-KIND:region -- ) {: rid:CAD-KIND:region :}
   MIR-N@ 0 ?do
      i MIR-NODE-ID {: node:CAD-KIND:node-id :}
      node rid LMM-IN-REGION? if
         node MIR-MOVE? if node MIR-NODE-REF MVW-CHECK
         else
            node MIR-OP@ dup LMM-MM-OP? swap LMM-EPI-OP? or 0= if E-LMM-OP throw then
         then
      then
   loop
   rid LMM-MM-COUNT 1 <> if E-LMM-MULTIMM throw then ;

: LMM-FIND-MM ( CAD-KIND:region -- ) {: rid:CAD-KIND:region :}   \ the sole contraction node (count checked already)
   MIR-N@ 0 ?do
      i MIR-NODE-ID {: node:CAD-KIND:node-id :}
      node rid LMM-IN-REGION? node MIR-OP@ LMM-MM-OP? and if node LMM-MMNODE! then
   loop ;

\ ---- prologue guard: the contraction's operands must all be external ---------
\ An EW region-member whose output is a contraction operand is a fused PROLOGUE; the
\ epilogue-only v1 kernel cannot pre-transform A/B/bias, so such a region fails closed.
\ A dissolved FREE movement operand folds into the A-base K-loop offset (maki/move-view.f);
\ a NON-movement interior producer is a compute prologue the epilogue-only v1 kernel cannot run.
: LMM-CHECK-PROLOGUE ( -- )
   LMM-MMNODE@ {: mm:CAD-KIND:node-id :}  0 LMM-RID @ {: rid:CAD-KIND:region :}
   mm MIR-IN-COUNT@ 0 ?do
      mm i MIR-INPUT-IDX MIR-IN@ {: ref:MIR:operand-ref :}
      ref MIR-REF-INPUT? 0= if
         ref MVW-DISSOLVED? if ref MVW-CHECK            \ folded free-movement operand: allowed
         else ref MIR-REF-NODE FP-RID@ rid FP-RGN= if E-LMM-PROLOGUE throw then then
      then
   loop ;

\ ---- contraction operands become the kernel inputs (A, B, [bias]) -----------
: LMM-COLLECT-INS ( -- )
   LMM-MMNODE@ {: mm:CAD-KIND:node-id :}
   mm MIR-IN-COUNT@ {: c:n :}
   c LMM-MAX-IN > if E-LMM-REG throw then
   c LMM-NIN !
   c 0 ?do  mm i MIR-INPUT-IDX MIR-IN@ i LMM-REF!  loop ;

\ ---- single materialized output --------------------------------------------
\ The planner plans EXACTLY ONE materialized output per region (linear operand-0 chain, the
\ tail its sole materialized node; proof + fan-out battery in maki/fusion-mout-test.f).
\ LMM-FIND-OUT asserts that invariant as defense-in-depth: != 1 is a corrupted plan (>1
\ structurally impossible, 0 a materialization-flag regression), never a v1 cap.
: LMM-MAT-COUNT ( CAD-KIND:region -- n ) {: rid:CAD-KIND:region :}
   0 MIR-N@ 0 ?do
      i MIR-NODE-ID {: node:CAD-KIND:node-id :}
      node rid LMM-IN-REGION? node MIR-MAT@ and if 1+ then
   loop ;
: LMM-FIND-OUT ( CAD-KIND:region -- ) {: rid:CAD-KIND:region :}
   rid LMM-MAT-COUNT 1 <> if E-LMM-MULTIOUT throw then
   MIR-N@ 0 ?do
      i MIR-NODE-ID {: node:CAD-KIND:node-id :}
      node rid LMM-IN-REGION? node MIR-MAT@ and if node LMM-OUTNODE! then
   loop ;

\ ---- operand-ref shape (input slot or interior producer) --------------------
: LMM-REF-ROWS ( MIR:operand-ref -- CAD-KIND:rows ) {: ref:MIR:operand-ref :}
   ref MIR-REF-INPUT? if ref MIR-REF-SLOT MIR-SLOT-ROWS@ else ref MIR-REF-NODE MIR-ROWS@ then ;
: LMM-REF-COLS ( MIR:operand-ref -- CAD-KIND:cols ) {: ref:MIR:operand-ref :}
   ref MIR-REF-INPUT? if ref MIR-REF-SLOT MIR-SLOT-COLS@ else ref MIR-REF-NODE MIR-COLS@ then ;
: LMM-REF-NROWS ( MIR:operand-ref -- n )  LMM-REF-ROWS ROWS-RAW ;
: LMM-REF-NCOLS ( MIR:operand-ref -- n )  LMM-REF-COLS COLS-RAW ;

\ ---- dims (M/N from the contraction output; K from A's cols) ----------------
: LMM-SET-DIMS ( -- )
   LMM-MMNODE@ {: mm:CAD-KIND:node-id :}
   mm MIR-ROWS@ ROWS-RAW LMM-M !
   mm MIR-COLS@ COLS-RAW LMM-N !
   0 LMM-REF@ LMM-REF-NCOLS LMM-K ! ;

\ shapes must be self-consistent (defense-in-depth: A MxK, B KxN, bias 1xN) ----
: LMM-CHECK-SHAPES ( -- )
   LMM-M @ {: M:n :}  LMM-N @ {: N:n :}  LMM-K @ {: K:n :}
   0 LMM-REF@ {: a:MIR:operand-ref :}
   a LMM-REF-NROWS M <>  a LMM-REF-NCOLS K <>  or if E-LMM-DIMS throw then
   1 LMM-REF@ {: b:MIR:operand-ref :}
   b LMM-REF-NROWS K <>  b LMM-REF-NCOLS N <>  or if E-LMM-DIMS throw then
   LMM-NIN @ 3 = if
      2 LMM-REF@ {: bias:MIR:operand-ref :}
      bias LMM-REF-NROWS 1 <>  bias LMM-REF-NCOLS N <>  or if E-LMM-DIMS throw then
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
: LMM-ANALYZE ( CAD-KIND:region -- ) {: rid:CAD-KIND:region :}
   rid FP-REGION-MEMBERS drop                    \ validates FP-BUILD ran + rid range
   rid 0 LMM-RID !
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
: LMM-IN-REF@ ( n -- MIR:operand-ref ) {: i:n :}
   i 0 < i LMM-NIN @ >= or if E-LMM-REG throw then  i LMM-REF@ ;
: LMM-IN-ELEMS@ ( n -- n ) {: i:n :}             \ input i's element count (A/B/bias differ)
   i LMM-IN-REF@ {: ref:MIR:operand-ref :}
   ref LMM-REF-ROWS ref LMM-REF-COLS SHAPE-ELEMS DIM-RAW ;
: LMM-OUT-NODE@ ( -- CAD-KIND:node-id )  LMM-OUTNODE@ ;
: LMM-M@ ( -- n )         LMM-M @ ;
: LMM-N@ ( -- n )         LMM-N @ ;
: LMM-K@ ( -- n )         LMM-K @ ;
: LMM-ELEMS ( -- n )      LMM-M @ LMM-N @ * ;
: LMM-RID@ ( -- CAD-KIND:region )  0 LMM-RID @ ;
: LMM-BLOCKED? ( -- bool ) LMM-BLK @ 0= 0= ;    \ register-blocked tile chosen for this shape?
: LMM-OUT-TILE@ ( -- n )  LMM-BLK @ if MM-BM else LMM-TILE then ;  \ launch-grid output-tile edge

private

\ ---- register map (member node result reg; epilogue operand resolution) -----
: LMM-NR@ ( CAD-KIND:node-id -- n ) {: nd:CAD-KIND:node-id :}
   nd NODE>RAW {: raw:n :}
   raw 0 < raw LMM-NCAP >= or if E-LMM-REG throw then  LMM-NODE-REG raw cells + @ ;
: LMM-NR! ( n CAD-KIND:node-id -- ) {: r:n nd:CAD-KIND:node-id :}
   nd NODE>RAW {: raw:n :}
   raw 0 < raw LMM-NCAP >= or if E-LMM-REG throw then  r LMM-NODE-REG raw cells + ! ;

\ a unary epilogue node reads operand-0 (the contraction or a prior epilogue reg)
: LMM-EPI-OPREG ( CAD-KIND:node-id -- n ) {: nd:CAD-KIND:node-id :}
   nd 0 MIR-INPUT-IDX MIR-IN@ MIR-REF-NODE LMM-NR@ ;
: LMM-EPI-NODE ( CAD-KIND:node-id -- ) {: nd:CAD-KIND:node-id :}
   nd MIR-OP@ MATCH opkind
      relu OF nd LMM-EPI-OPREG EMIT-RELU ENDOF
      gelu OF nd LMM-EPI-OPREG EMIT-GELU ENDOF
      silu OF nd LMM-EPI-OPREG EMIT-SILU ENDOF
      add OF E-LMM-OP throw ENDOF  mul OF E-LMM-OP throw ENDOF
      scale OF E-LMM-OP throw ENDOF  bias OF E-LMM-OP throw ENDOF
      layernorm OF E-LMM-OP throw ENDOF  rmsnorm OF E-LMM-OP throw ENDOF
      softmax-row OF E-LMM-OP throw ENDOF  matmul OF E-LMM-OP throw ENDOF
      linear OF E-LMM-OP throw ENDOF  residual-add OF E-LMM-OP throw ENDOF
      cast OF E-LMM-OP throw ENDOF  rope OF E-LMM-OP throw ENDOF
      reshape OF E-LMM-OP throw ENDOF  transpose OF E-LMM-OP throw ENDOF
      slice OF E-LMM-OP throw ENDOF  concat OF E-LMM-OP throw ENDOF  gather OF E-LMM-OP throw ENDOF
      relu-bwd OF E-LMM-OP throw ENDOF  gelu-bwd OF E-LMM-OP throw ENDOF  silu-bwd OF E-LMM-OP throw ENDOF
      layernorm-bwd OF E-LMM-OP throw ENDOF  rmsnorm-bwd OF E-LMM-OP throw ENDOF
      softmax-row-bwd OF E-LMM-OP throw ENDOF  rope-bwd OF E-LMM-OP throw ENDOF
      rowsum-bwd OF E-LMM-OP throw ENDOF  fullsum-dot-bwd OF E-LMM-OP throw ENDOF
      pad-scatter OF E-LMM-OP throw ENDOF  scatter-add OF E-LMM-OP throw ENDOF
      gelu-bwd2 OF E-LMM-OP throw ENDOF
   ;MATCH
   nd LMM-NR! ;
: LMM-EPI-CHAIN ( -- )                           \ epilogue nodes in topo order (skip folded movement)
   MIR-N@ 0 ?do
      i MIR-NODE-ID {: node:CAD-KIND:node-id :}
      node 0 LMM-RID @ LMM-IN-REGION?  node LMM-MMNODE@ MIR-NODE= 0=  and
      node MIR-MOVE? 0= and if node LMM-EPI-NODE then
   loop ;

\ fold each dissolved free-movement contraction operand into a base-pointer offset (reshape 0 /
\ slice r0*K*4): the operand's global pointer is advanced after LMM-PARAMS cvta's it, so the
\ K-loop reads the movement's source window with no other change (maki/move-view.f).
: LMM-APPLY-VIEWS ( -- )
   LMM-NIN @ 0 ?do
      i LMM-REF@ MVW-RESOLVE-OFF {: off:n :}
      off 0 > if
         SB-RESET s" add.u64 %rd" CG-S i 1+ SB-U s" , %rd" CG-S i 1+ SB-U s" , " CG-S off SB-U s" ;" CG-S CG-LINE
      then
   loop ;

\ ---- entry / regs / params (K inputs + output + M,N,K) ----------------------
\ RGN>RAW is the one kernel-name render boundary (REGION_<rid>)
: LMM-KNAME ( -- )  s" REGION_" CG-S 0 LMM-RID @ RGN>RAW SB-U ;
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
   LMM-OUTNODE@ LMM-NR@ {: outf:n :}
   s" mad.lo.u32 %r13, %r7, %r2, %r11;" PTX-L
   s" mul.wide.u32 %rd10, %r13, 4;" PTX-L
   SB-RESET s" add.u64 %rd11, %rd" CG-S LMM-OUT-BASE SB-U s" , %rd10;" CG-S CG-LINE
   SB-RESET s" st.global.f32 [%rd11], %f" CG-S outf SB-U s" ;" CG-S CG-LINE ;

: LMM-BODY ( -- )
   LMM-COORDS
   LMM-KLOOP
   LMM-MMNODE@ MIR-OP@ MAKI-OPKIND:LINEAR MAKI-OPKIND:EQ if LMM-BIAS then
   1 LMM-MMNODE@ LMM-NR!                          \ contraction result = %f1 (after any bias)
   LMM-RESET-REGS
   LMM-EPI-CHAIN
   LMM-STORE ;

\ ---- blocked (register-tiled 64x64) body: reuse the cg-matmul.f staging/compute vocabulary --
\ MM-THREAD-SETUP (r4..r11), MM-ACC-ZERO-EMIT (%f10..%f25 = 0), and the cp.async double-buffered
\ MM-PIPE-KLOOP (per-buffer r12/r13 bases + %f26..%f33 loads + 4x4 FMAs) are emitted verbatim; A=%rd1, B=%rd2 match
\ this ABI. Only the K-loop scaffold and the epilogue write are re-expressed for fusion here.
: LMM-BLK-SHARED ( -- )                            \ 2x .shared As[64][32]+Bs[32][64] (cp.async double-buffer)
   SB-RESET s" .shared .align 16 .b8 SH[" CG-S MM-SMEM SB-U s" ];" CG-S CG-LINE ;

\ The cp.async double-buffered K-loop is cg-matmul.f MM-PIPE-KLOOP verbatim (a ( -- ) emitter;
\ cg-matmul's MM-K-LOOP wraps the same word with the mmctx/mmacc token passthrough this
\ non-KERNEL lowering does not thread). Sharing it keeps the device goldens validating the
\ exact kernel gemm-bench times.
: LMM-BLK-KLOOP ( -- )  MM-PIPE-KLOOP ;

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
   LMM-MMNODE@ MIR-OP@ MAKI-OPKIND:LINEAR MAKI-OPKIND:EQ if                                   \ bias fusion: acc += bias[cCol]
      s" mul.wide.u32 %rd10, %r43, 4;" PTX-L
      s" add.u64 %rd11, %rd3, %rd10;" PTX-L
      s" ld.global.f32 %f4, [%rd11];" PTX-L
      SB-RESET s" add.rn.f32 %f5, %f" CG-S accf SB-U s" , %f4;" CG-S CG-LINE
      5
   else accf then {: srcf:n :}
   40 CG-NF !                                                            \ epilogue temps above tile/staging regs
   srcf LMM-MMNODE@ LMM-NR!
   LMM-EPI-CHAIN
   LMM-OUTNODE@ LMM-NR@ {: outf:n :}
   s" mul.wide.u32 %rd10, %r44, 4;" PTX-L
   SB-RESET s" add.u64 %rd11, %rd" CG-S LMM-OUT-BASE SB-U s" , %rd10;" CG-S CG-LINE
   SB-RESET s" st.global.f32 [%rd11], %f" CG-S outf SB-U s" ;" CG-S CG-LINE ;

: LMM-BLK-WRITE ( -- )                             \ fused epilogue over the thread's 4x4 micro-tile
   LMM-BLK-CBASE
   MM-TM 0 ?do  MM-TM 0 ?do  j i LMM-BLK-ELEM  loop  loop ;

: LMM-BLK-BODY ( -- )
   MM-THREAD-SETUP
   MM-ACC-ZERO-EMIT
   LMM-BLK-KLOOP                                   \ MM-PIPE-KLOOP sets the per-buffer As/Bs bases itself
   LMM-BLK-WRITE ;

\ ---- TF32 tensor-core (mma.sync) blocked body: reuse cg-mma.f's warp-tiled MMA compute --
\ Same 64x64 block / cp.async double-buffer as LMM-BLK-BODY (MM-THREAD-SETUP / MM-ACC-ZERO /
\ MM-PIPE-KLOOP-WITH), but the compute quotation is cg-mma.f MMA-KTILE (warp-level MMA tiles)
\ instead of the 4x4 fma, and the epilogue write folds bias+activation per MMA D-fragment
\ element. Chosen only when the matmul class is LICENSED at TF32 (LMM-MMA?), so the f32 path
\ (and its f32-tolerance goldens) is unchanged; the MMA path is judged under the tf32 row.
: LMM-MMA? ( -- bool )  LMM-BLK @ 0<>  CLASS-MATMUL PREC@ PREC-TF32 =  and ;

\ one MMA D-fragment element: row reg %r<rr>, col reg %r<cr>, accumulator %f<accf> -> fold
\ bias[col] (LINEAR) -> activation chain -> store to out[row*N+col]. Mirrors LMM-BLK-ELEM but
\ over the mma.sync (gRow0/gRow1, col0/col1) output mapping instead of the contiguous 4x4 tile.
: LMM-MMA-ELEM ( n n n -- ) {: rr:n cr:n accf:n :}
   SB-RESET s" mad.lo.u32 %r44, %r" CG-S rr SB-U s" , %r2, %r" CG-S cr SB-U s" ;" CG-S CG-LINE  \ gidx = row*N + col
   LMM-MMNODE@ MIR-OP@ MAKI-OPKIND:LINEAR MAKI-OPKIND:EQ if                                   \ bias fusion: acc += bias[col]
      SB-RESET s" mul.wide.u32 %rd10, %r" CG-S cr SB-U s" , 4;" CG-S CG-LINE
      s" add.u64 %rd11, %rd3, %rd10;" PTX-L
      s" ld.global.f32 %f4, [%rd11];" PTX-L
      SB-RESET s" add.rn.f32 %f5, %f" CG-S accf SB-U s" , %f4;" CG-S CG-LINE
      5
   else accf then {: srcf:n :}
   40 CG-NF !                                                            \ epilogue temps above tile/staging regs
   srcf LMM-MMNODE@ LMM-NR!
   LMM-EPI-CHAIN
   LMM-OUTNODE@ LMM-NR@ {: outf:n :}
   s" mul.wide.u32 %rd10, %r44, 4;" PTX-L
   SB-RESET s" add.u64 %rd11, %rd" CG-S LMM-OUT-BASE SB-U s" , %rd10;" CG-S CG-LINE
   SB-RESET s" st.global.f32 [%rd11], %f" CG-S outf SB-U s" ;" CG-S CG-LINE ;

\ one warp n-tile j (cols warp_col*32 + j*8): compute col0/col1 registers, then the 4 D
\ elements. %r32=gRow0 %r33=gRow1 %r34=gCol0 come from cg-mma.f MMA-SETUP (survive the K-loop).
: LMM-MMA-NTILE ( n -- ) {: j:n :}
   SB-RESET s" add.u32 %r40, %r34, " CG-S j 8 * SB-U s" ;" CG-S CG-LINE   \ col0 = gCol0 + j*8
   s" add.u32 %r41, %r40, 1;" PTX-L                                       \ col1 = col0 + 1
   10 j 4 * + {: a0:n :}                                                  \ tile-j accumulators %f(10+4j)..
   32 40 a0     LMM-MMA-ELEM                                              \ d0 = D[gRow0][col0]
   32 41 a0 1+  LMM-MMA-ELEM                                              \ d1 = D[gRow0][col1]
   33 40 a0 2 + LMM-MMA-ELEM                                              \ d2 = D[gRow1][col0]
   33 41 a0 3 + LMM-MMA-ELEM ;                                            \ d3 = D[gRow1][col1]

: LMM-MMA-WRITE ( -- )  4 0 ?do  i LMM-MMA-NTILE  loop ;                  \ 4 n-tiles / warp

: LMM-MMA-BODY ( -- )
   MM-THREAD-SETUP
   MM-ACC-ZERO-EMIT
   MMA-SETUP
   [: MMA-KTILE ;] MM-PIPE-KLOOP-WITH
   LMM-MMA-WRITE ;

public
\ LMM-EMIT prints the region's PTX module to the current PTX sink (stdout, or the
\ in-process capture buffer). Analysis first, so a rejected region emits nothing.
: LMM-EMIT ( CAD-KIND:region -- )
   LMM-ANALYZE
   PTX-MODULE{
      LMM-ENTRY  LMM-OPEN
      LMM-BLK @ if LMM-BLK-SHARED then
      LMM-PARAMS  LMM-APPLY-VIEWS
      LMM-MMA? if LMM-MMA-BODY else
         LMM-BLK @ if LMM-BLK-BODY else LMM-BODY then
      then
      s" DONE:" PTX-L
      s" ret;" PTX-L
      s" }" PTX-L
   }PTX-MODULE ;

;package
