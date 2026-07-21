\ cg-collective.f - PTX codegen: emit-mode lowering for the M6 row/collective ops.
\
\ The emit dual of lib/ptx/collective.f, exactly as lib/ptx/cg.f is for the M4
\ tile ops: each op's runtime value is a PTX register NUMBER, and running a
\ checked KERNEL: body in emit mode produces its PTX. One block per row; the
\ block reduction (BLOCK-MAX / BLOCK-SUM) is a two-level warp reduction
\ (full-warp shfl.sync.down tree + per-warp shared staging + a final warp
\ reduce), not an O(B) fold. Each collective applies its own inactive-lane
\ identity before writing shared memory, so direct sums and backward cotangents
\ do not inherit ROW-LOAD's max-friendly -inf seed. Param registers (CG-SM-PARAMS):
\ in=%rd1, out=%rd2, k=%r1. Load after lib/errors.f, lib/string.f, lib/fmt.f,
\ src/arch/ptx/emit.f, lib/ptx/header.f, and lib/ptx/cg.f. Checked Habu.

: CG-BLOCK ( -- n )
   PTX-BLOCK@ dup PTX-BLOCK-CHECK ;

: SMEM-BYTES ( -- n )
   CG-BLOCK 4 * ;

: CG-REDUCE-OP$ ( n -- ptr u8 n )
   case
      0 of s" max.f32 " endof
      1 of s" add.f32 " endof
      2 of s" min.f32 " endof
      drop s" cg: unknown reduce op" 76 die
   endcase ;

: CG-REDUCE-ID$ ( n -- ptr u8 n )
   case
      0 of s" 0fFF800000" endof
      1 of s" 0f00000000" endof
      2 of s" 0f7F800000" endof
      drop s" cg: unknown reduce op" 76 die
   endcase ;

\ --- softmax entry scaffolding (distinct from cg.f's SAXPY scaffolding) ---
: CG-SM-RESET ( -- )  1 CG-NF !  3 CG-NRD !  2 CG-NR !  1 CG-NP !  0 CG-NL ! ;
: CG-SM-ENTRY ( -- )
   s" .visible .entry SOFTMAX_ROWS(.param .u64 p_in, .param .u64 p_out, .param .u32 p_k)" PTX-L ;
: CG-SM-OPEN ( -- )
   s" {" PTX-L
   s" .reg .pred %p<32>;" PTX-L
   s" .reg .f32 %f<64>;" PTX-L
   s" .reg .b32 %r<64>;" PTX-L
   s" .reg .b64 %rd<32>;" PTX-L
   SB-RESET s" .shared .align 4 .b8 SMEM[" CG-S SMEM-BYTES FMT:SB-U s" ];" CG-S CG-LINE ;
: CG-SM-PARAMS ( -- )
   s" ld.param.u64 %rd1, [p_in];" PTX-L
   s" ld.param.u64 %rd2, [p_out];" PTX-L
   s" ld.param.u32 %r1, [p_k];" PTX-L ;
: CG-SM-RET ( -- )    s" ret;" PTX-L ;
: CG-SM-CLOSE ( -- )  s" }" PTX-L ;

\ backward entry: x (input), dy (incoming cotangent), out (dx); x=%rd1 dy=%rd2
\ out=%rd3 k=%r1.  Reuses CG-SM-OPEN/RET/CLOSE.
: CG-BW-RESET ( -- )  1 CG-NF !  4 CG-NRD !  2 CG-NR !  1 CG-NP !  0 CG-NL ! ;
: CG-BW-ENTRY ( -- )
   s" .visible .entry SOFTMAX_BWD(.param .u64 p_x, .param .u64 p_dy, .param .u64 p_out, .param .u32 p_k)" PTX-L ;
: CG-BW-PARAMS ( -- )
   s" ld.param.u64 %rd1, [p_x];" PTX-L
   s" ld.param.u64 %rd2, [p_dy];" PTX-L
   s" ld.param.u64 %rd3, [p_out];" PTX-L
   s" ld.param.u32 %r1, [p_k];" PTX-L ;

\ --- per-op emitters (register numbers) ---
\ ROW: r = blockIdx.x (one block per row).
: EMIT-ROW ( -- n )
   CG-NEXT-R {: r :}
   SB-RESET s" mov.u32 " CG-S r CG-R s" , %ctaid.x;" CG-S CG-LINE
   r ;

\ ROW-SPAN: rowbase = cvta.global(base) + row*k*4   (k=%r1).
: EMIT-ROW-SPAN ( n n -- n ) {: base row :}
   CG-NEXT-R {: rm :}
   SB-RESET s" mul.lo.s32 " CG-S rm CG-R s" , " CG-S row CG-R s" , %r1;" CG-S CG-LINE
   CG-NEXT-RD {: off :}
   SB-RESET s" mul.wide.u32 " CG-S off CG-RD s" , " CG-S rm CG-R s" , 4;" CG-S CG-LINE
   CG-NEXT-RD {: g :}
   SB-RESET s" cvta.to.global.u64 " CG-S g CG-RD s" , " CG-S base CG-RD s" ;" CG-S CG-LINE
   CG-NEXT-RD {: rb :}
   SB-RESET s" add.u64 " CG-S rb CG-RD s" , " CG-S g CG-RD s" , " CG-S off CG-RD s" ;" CG-S CG-LINE
   rb ;

: EMIT-ROW-SPAN-ONCE ( n n -- n )
   EMIT-ROW-SPAN ;

\ ROW-SPAN0: rowbase pinned to row 0 = cvta.global(base) (no row*k advance). A 1xC / 1x1
\ broadcast operand has a single row, so every block reads the SAME row; ROW-LOAD then
\ reads element tid (1xC) or, with a zero col offset, element 0 (1x1). Mirrors the
\ executor EX-BC@ row-0 read for a broadcast operand (maki/bcast.f).
: EMIT-ROW-SPAN0 ( n -- n ) {: base:n :}
   CG-NEXT-RD {: g:n :}
   SB-RESET s" cvta.to.global.u64 " CG-S g CG-RD s" , " CG-S base CG-RD s" ;" CG-S CG-LINE
   g ;

\ ROW-SPAN-STRIDE1: rowbase = cvta.global(base) + row*4 (stride 1, not k). An Rx1
\ column operand holds ONE element per row, so block r's span starts at element r;
\ ROW-LOAD with a zero col offset then reads element r in every lane. Mirrors the
\ executor EX-BC@ Rx1 read [e / C] (maki/bcast.f BC-COL).
: EMIT-ROW-SPAN-STRIDE1 ( n n -- n ) {: base:n row:n :}
   CG-NEXT-RD {: off:n :}
   SB-RESET s" mul.wide.u32 " CG-S off CG-RD s" , " CG-S row CG-R s" , 4;" CG-S CG-LINE
   CG-NEXT-RD {: g:n :}
   SB-RESET s" cvta.to.global.u64 " CG-S g CG-RD s" , " CG-S base CG-RD s" ;" CG-S CG-LINE
   CG-NEXT-RD {: rb:n :}
   SB-RESET s" add.u64 " CG-S rb CG-RD s" , " CG-S g CG-RD s" , " CG-S off CG-RD s" ;" CG-S CG-LINE
   rb ;

\ ROW-CTX: per-thread column byte offset = tid*4 (span unused; bounds recomputed
\ at load/store from %tid.x and %r1).
: EMIT-ROW-CTX ( n -- n ) {: span :}
   CG-NEXT-R {: rt :}
   SB-RESET s" mov.u32 " CG-S rt CG-R s" , %tid.x;" CG-S CG-LINE
   CG-NEXT-RD {: c :}
   SB-RESET s" mul.wide.u32 " CG-S c CG-RD s" , " CG-S rt CG-R s" , 4;" CG-S CG-LINE
   c ;

: EMIT-ROW-CTX-ONCE ( n -- n )
   EMIT-ROW-CTX ;

\ ROW-LOAD: masked load from rowbase+coloff; inactive lanes seed -inf.
: EMIT-ROW-LOAD ( n n -- n ) {: span ctx :}
   CG-NEXT-RD {: a :}
   SB-RESET s" add.u64 " CG-S a CG-RD s" , " CG-S span CG-RD s" , " CG-S ctx CG-RD s" ;" CG-S CG-LINE
   CG-NEXT-R {: rt :}
   SB-RESET s" mov.u32 " CG-S rt CG-R s" , %tid.x;" CG-S CG-LINE
   CG-NEXT-P {: p :}
   SB-RESET s" setp.lt.u32 " CG-S p CG-P s" , " CG-S rt CG-R s" , %r1;" CG-S CG-LINE
   CG-NEXT-F {: t :}
   SB-RESET s" mov.f32 " CG-S t CG-F s" , 0fFF800000;" CG-S CG-LINE
   SB-RESET s" @" CG-S p CG-P s"  ld.global.f32 " CG-S t CG-F s" , [" CG-S a CG-RD s" ];" CG-S CG-LINE
   t ;

: EMIT-ROW-LOAD-ONCE ( n n -- n )
   EMIT-ROW-LOAD ;

\ --- warp-level block reduction (shfl.sync.down + per-warp shared staging) -------
\ The block reduce is the standard two-level pattern, replacing the O(B) thread-0
\ fold: (1) each lane seeds active?tile:identity; (2) a full-warp membermask
\ shfl.down tree (offsets 16..1) reduces each 32-lane warp so its lane 0 holds the
\ warp partial; (3) a leading reuse bar.sync (WAR-guards SMEM against a prior
\ reduce in the same kernel, see EMIT-STAGE-PARTIAL) then lane 0 of each warp
\ stages that partial to SMEM[warp], one bar.sync; (4) warp 0 loads the
\ CG-WARP-COUNT partials (identity past the count),
\ shfl-reduces them, and lane 0 writes the block result to SMEM[0]; (5) one
\ bar.sync then every lane loads SMEM[0]. Blocks are always a warp multiple
\ (PTX-BLOCK-LEGAL?), so all 32 lanes of every warp participate and the -1
\ membermask is always well-formed; the identity seed threads through both shuffle
\ levels so inactive lanes (tid>=%r1) and past-the-count final lanes contribute the
\ reducer identity, never garbage.
: CG-WARP-COUNT ( -- n )  CG-BLOCK PTX-WARP / ;         \ warps per block = B/32

\ one tree step: t = shfl.down(vf, off, full warp); vf = op(vf, t). vf/t are
\ redefined across steps (non-SSA text; ptxas re-SSAs), so one temp t per reduce.
: WARP-SHFL-STEP ( n n n n -- ) {: off:n vf:n op:n t:n :}
   SB-RESET s" shfl.sync.down.b32 " CG-S t CG-F s" , " CG-S vf CG-F s" , " CG-S off FMT:SB-U
      s" , " CG-S PTX-WARP 1 - FMT:SB-U s" , -1;" CG-S CG-LINE
   SB-RESET op CG-REDUCE-OP$ CG-S vf CG-F s" , " CG-S vf CG-F s" , " CG-S t CG-F s" ;" CG-S CG-LINE ;

\ 32-lane halving tree (PTX-WARP=32): after it, lane 0 of the warp holds the reduce.
: WARP-SHFL-REDUCE ( n n -- ) {: vf:n op:n :}
   CG-NEXT-F {: t:n :}
   16 vf op t WARP-SHFL-STEP   8 vf op t WARP-SHFL-STEP   4 vf op t WARP-SHFL-STEP
    2 vf op t WARP-SHFL-STEP   1 vf op t WARP-SHFL-STEP ;

\ seed the per-lane reduce value: v = (tid<%r1) ? tile : identity. Returns v, tid.
: EMIT-REDUCE-SEED ( n n -- n n ) {: tile:n op:n :}
   CG-NEXT-R {: rt:n :}
   SB-RESET s" mov.u32 " CG-S rt CG-R s" , %tid.x;" CG-S CG-LINE
   CG-NEXT-P {: pact:n :}
   SB-RESET s" setp.lt.u32 " CG-S pact CG-P s" , " CG-S rt CG-R s" , %r1;" CG-S CG-LINE
   CG-NEXT-F {: v:n :}
   SB-RESET s" mov.f32 " CG-S v CG-F s" , " CG-S op CG-REDUCE-ID$ CG-S s" ;" CG-S CG-LINE
   SB-RESET s" @" CG-S pact CG-P s"  mov.f32 " CG-S v CG-F s" , " CG-S tile CG-F s" ;" CG-S CG-LINE
   v rt ;

\ lane id (tid & 31) and warp id (tid >> 5) for a 1-D block.
: EMIT-WARP-IDS ( n -- n n ) {: rt:n :}
   CG-NEXT-R {: rlane:n :}
   SB-RESET s" and.b32 " CG-S rlane CG-R s" , " CG-S rt CG-R s" , " CG-S PTX-WARP 1 - FMT:SB-U s" ;" CG-S CG-LINE
   CG-NEXT-R {: rwarp:n :}
   SB-RESET s" shr.u32 " CG-S rwarp CG-R s" , " CG-S rt CG-R s" , 5;" CG-S CG-LINE
   rlane rwarp ;

\ Leading reuse barrier (WAR): a prior reduce in the same kernel ends with
\ EMIT-BCAST-SMEM0's `ld.shared [SMEM]` on every lane; this word's stage-write
\ clobbers SMEM (warp 0's lane 0 hits SMEM[0]). Read and write are on different
\ threads (warp k reads, warp 0 writes) with no barrier between them, so the PTX
\ memory consistency model orders them only through a CTA-scope barrier - without
\ one it is a write-after-read data race whose result is undefined, latent today
\ only because a CTA's warps run near-lockstep (empirically LN's 4-reduce backward,
\ softmax, rmsnorm, LRED-LN all gradcheck on the GB10, but that is a scheduling
\ property, not an architectural guarantee). The leading bar.sync forces every
\ prior SMEM read to finish before the reuse-write; it is the canonical "sync
\ before reusing shared memory" and is redundant-but-harmless on the first reduce.
\ Rule: PTX ISA "Memory Consistency Model" - two conflicting accesses (>=1 a write)
\ from different threads not ordered by the causality (happens-before) relation form
\ a data race with undefined result, and bar.sync is the CTA synchronization that
\ establishes that order. Paraphrased from training knowledge of
\ docs.nvidia.com/cuda/parallel-thread-execution, not a verbatim quote.
\ (dot habu-harden-emit-reduce-dc4eef6f)
\ warp lane 0 writes its partial to SMEM[warp]; then bar.sync stages all partials.
: EMIT-STAGE-PARTIAL ( n n n -- ) {: v:n rlane:n rwarp:n :}
   SB-RESET s" bar.sync 0;" CG-S CG-LINE     \ WAR reuse barrier (see comment above)
   CG-NEXT-R {: rsm:n :}
   SB-RESET s" mov.u32 " CG-S rsm CG-R s" , SMEM;" CG-S CG-LINE
   CG-NEXT-R {: roff:n :}
   SB-RESET s" shl.b32 " CG-S roff CG-R s" , " CG-S rwarp CG-R s" , 2;" CG-S CG-LINE
   CG-NEXT-R {: ra:n :}
   SB-RESET s" add.s32 " CG-S ra CG-R s" , " CG-S rsm CG-R s" , " CG-S roff CG-R s" ;" CG-S CG-LINE
   CG-NEXT-P {: plead:n :}
   SB-RESET s" setp.eq.u32 " CG-S plead CG-P s" , " CG-S rlane CG-R s" , 0;" CG-S CG-LINE
   SB-RESET s" @" CG-S plead CG-P s"  st.shared.f32 [" CG-S ra CG-R s" ], " CG-S v CG-F s" ;" CG-S CG-LINE
   SB-RESET s" bar.sync 0;" CG-S CG-LINE ;

\ warp 0 reduces the CG-WARP-COUNT partials (identity past the count) and lane 0
\ writes the block result to SMEM[0]; the guard branch is warp-uniform (no
\ intra-warp divergence), so warp 0's shfl keeps a full 32-lane membermask.
: EMIT-FINAL-REDUCE ( n n n -- ) {: op:n rt:n rwarp:n :}
   CG-NEXT-L {: lskip:n :}
   CG-NEXT-P {: pne:n :}
   SB-RESET s" setp.ne.u32 " CG-S pne CG-P s" , " CG-S rwarp CG-R s" , 0;" CG-S CG-LINE
   SB-RESET s" @" CG-S pne CG-P s"  bra " CG-S lskip CG-L s" ;" CG-S CG-LINE
   CG-NEXT-P {: plt:n :}
   SB-RESET s" setp.lt.u32 " CG-S plt CG-P s" , " CG-S rt CG-R s" , " CG-S CG-WARP-COUNT FMT:SB-U s" ;" CG-S CG-LINE
   CG-NEXT-F {: v2:n :}
   SB-RESET s" mov.f32 " CG-S v2 CG-F s" , " CG-S op CG-REDUCE-ID$ CG-S s" ;" CG-S CG-LINE
   CG-NEXT-R {: rsm:n :}
   SB-RESET s" mov.u32 " CG-S rsm CG-R s" , SMEM;" CG-S CG-LINE
   CG-NEXT-R {: roff:n :}
   SB-RESET s" shl.b32 " CG-S roff CG-R s" , " CG-S rt CG-R s" , 2;" CG-S CG-LINE
   CG-NEXT-R {: ra:n :}
   SB-RESET s" add.s32 " CG-S ra CG-R s" , " CG-S rsm CG-R s" , " CG-S roff CG-R s" ;" CG-S CG-LINE
   SB-RESET s" @" CG-S plt CG-P s"  ld.shared.f32 " CG-S v2 CG-F s" , [" CG-S ra CG-R s" ];" CG-S CG-LINE
   v2 op WARP-SHFL-REDUCE
   CG-NEXT-L {: lwritten:n :}
   CG-NEXT-P {: pne0:n :}
   SB-RESET s" setp.ne.u32 " CG-S pne0 CG-P s" , " CG-S rt CG-R s" , 0;" CG-S CG-LINE
   SB-RESET s" @" CG-S pne0 CG-P s"  bra " CG-S lwritten CG-L s" ;" CG-S CG-LINE
   SB-RESET s" st.shared.f32 [SMEM], " CG-S v2 CG-F s" ;" CG-S CG-LINE
   lwritten CG-LDEF
   lskip CG-LDEF
   SB-RESET s" bar.sync 0;" CG-S CG-LINE ;

\ every lane loads the broadcast block result from SMEM[0].
: EMIT-BCAST-SMEM0 ( -- n )
   CG-NEXT-F {: u:n :}
   SB-RESET s" ld.shared.f32 " CG-S u CG-F s" , [SMEM];" CG-S CG-LINE
   u ;

\ warp-shfl block reduction; the result is broadcast to every lane. op: 0=max 1=add 2=min.
: EMIT-REDUCE ( n n -- n ) {: tile:n op:n :}
   tile op EMIT-REDUCE-SEED {: v:n rt:n :}
   rt EMIT-WARP-IDS {: rlane:n rwarp:n :}
   v op WARP-SHFL-REDUCE
   v rlane rwarp EMIT-STAGE-PARTIAL
   op rt rwarp EMIT-FINAL-REDUCE
   EMIT-BCAST-SMEM0 ;

: EMIT-BLOCK-MAX ( n -- n )  0 EMIT-REDUCE ;
: EMIT-BLOCK-SUM ( n -- n )  1 EMIT-REDUCE ;
: EMIT-BLOCK-MIN ( n -- n )  2 EMIT-REDUCE ;          \ used by BLOCK-MAX-SELECT

\ BLOCK-MAX-SELECT (the BLOCK-MAX adjoint): route the cotangent ds to the LOWEST
\ lane where x==mx and 0 elsewhere. candidate = (x==mx) ? float(tid) : +inf; the
\ arg-max lane is block-min(candidate); dx = (tid==argmax) ? ds : 0.
: EMIT-BLOCK-MAX-SELECT ( n n n -- n ) {: ds x mx :}
   CG-NEXT-R {: rt :}  CG-NEXT-F {: ftid :}
   SB-RESET s" mov.u32 " CG-S rt CG-R s" , %tid.x;" CG-S CG-LINE
   SB-RESET s" cvt.rn.f32.u32 " CG-S ftid CG-F s" , " CG-S rt CG-R s" ;" CG-S CG-LINE
   CG-NEXT-P {: peq :}  CG-NEXT-F {: fcand :}
   SB-RESET s" setp.eq.f32 " CG-S peq CG-P s" , " CG-S x CG-F s" , " CG-S mx CG-F s" ;" CG-S CG-LINE
   SB-RESET s" mov.f32 " CG-S fcand CG-F s" , 0f7F800000;" CG-S CG-LINE          \ +inf
   SB-RESET s" selp.f32 " CG-S fcand CG-F s" , " CG-S ftid CG-F s" , " CG-S fcand CG-F s" , " CG-S peq CG-P s" ;" CG-S CG-LINE
   fcand EMIT-BLOCK-MIN {: fargmax :}
   CG-NEXT-P {: psel :}  CG-NEXT-F {: fdx :}
   SB-RESET s" setp.eq.f32 " CG-S psel CG-P s" , " CG-S ftid CG-F s" , " CG-S fargmax CG-F s" ;" CG-S CG-LINE
   SB-RESET s" mov.f32 " CG-S fdx CG-F s" , 0f00000000;" CG-S CG-LINE             \ 0
   SB-RESET s" selp.f32 " CG-S fdx CG-F s" , " CG-S ds CG-F s" , " CG-S fdx CG-F s" , " CG-S psel CG-P s" ;" CG-S CG-LINE
   fdx ;

\ B- : tile - uniform (broadcast subtract)
: EMIT-B- ( n n -- n ) {: tile unif :}
   CG-NEXT-F {: r :}
   SB-RESET s" sub.f32 " CG-S r CG-F s" , " CG-S tile CG-F s" , " CG-S unif CG-F s" ;" CG-S CG-LINE
   r ;

\ EXP. : ex2.approx(z * log2e)
: EMIT-EXP ( n -- n ) {: tile :}
   CG-NEXT-F {: r :}
   SB-RESET s" mul.f32 " CG-S r CG-F s" , " CG-S tile CG-F s" , 0f3FB8AA3B;" CG-S CG-LINE
   SB-RESET s" ex2.approx.f32 " CG-S r CG-F s" , " CG-S r CG-F s" ;" CG-S CG-LINE
   r ;

\ B/ : tile / uniform (broadcast divide, IEEE round-to-nearest)
: EMIT-B/ ( n n -- n ) {: tile unif :}
   CG-NEXT-F {: r :}
   SB-RESET s" div.rn.f32 " CG-S r CG-F s" , " CG-S tile CG-F s" , " CG-S unif CG-F s" ;" CG-S CG-LINE
   r ;

\ U/ : uniform / uniform - the scalar divide the B/ adjoint needs (ds = -Sum(dz*z)/s)
: EMIT-U/ ( n n -- n ) {: a b :}
   CG-NEXT-F {: r :}
   SB-RESET s" div.rn.f32 " CG-S r CG-F s" , " CG-S a CG-F s" , " CG-S b CG-F s" ;" CG-S CG-LINE
   r ;

\ BROADCAST : uniform -> tile (every lane already holds the reduced scalar)
: EMIT-BROADCAST ( n -- n ) {: unif :}
   CG-NEXT-F {: r :}
   SB-RESET s" mov.f32 " CG-S r CG-F s" , " CG-S unif CG-F s" ;" CG-S CG-LINE
   r ;

\ NEG : tile -> -tile (used by the B-/B/ adjoints in the AD pass)
: EMIT-NEG ( n -- n ) {: tile :}
   CG-NEXT-F {: r :}
   SB-RESET s" neg.f32 " CG-S r CG-F s" , " CG-S tile CG-F s" ;" CG-S CG-LINE
   r ;

\ ZERO : a fresh zero tile (DROP's typed-zero adjoint in the AD pass)
: EMIT-ZERO ( -- n )
   CG-NEXT-F {: r:n :}
   SB-RESET s" mov.f32 " CG-S r CG-F s" , 0f00000000;" CG-S CG-LINE
   r ;

\ ROW-STORE : masked store to rowbase+coloff (active lanes only)
: EMIT-ROW-STORE ( n n n -- ) {: tile span ctx :}
   CG-NEXT-RD {: a :}
   SB-RESET s" add.u64 " CG-S a CG-RD s" , " CG-S span CG-RD s" , " CG-S ctx CG-RD s" ;" CG-S CG-LINE
   CG-NEXT-R {: rt :}
   SB-RESET s" mov.u32 " CG-S rt CG-R s" , %tid.x;" CG-S CG-LINE
   CG-NEXT-P {: p :}
   SB-RESET s" setp.lt.u32 " CG-S p CG-P s" , " CG-S rt CG-R s" , %r1;" CG-S CG-LINE
   SB-RESET s" @" CG-S p CG-P s"  st.global.f32 [" CG-S a CG-RD s" ], " CG-S tile CG-F s" ;" CG-S CG-LINE ;

: EMIT-ROW-STORE-ONCE ( n n n -- )
   EMIT-ROW-STORE ;

\ ROW-SCATTER-ADD : masked red.global.add.f32 to rowbase+coloff.
: EMIT-ROW-SCATTER-ADD ( n n n -- ) {: tile:n span:n ctx:n :}
   CG-NEXT-RD {: a:n :}
   SB-RESET s" add.u64 " CG-S a CG-RD s" , " CG-S span CG-RD s" , " CG-S ctx CG-RD s" ;" CG-S CG-LINE
   CG-NEXT-R {: rt:n :}
   SB-RESET s" mov.u32 " CG-S rt CG-R s" , %tid.x;" CG-S CG-LINE
   CG-NEXT-P {: p:n :}
   SB-RESET s" setp.lt.u32 " CG-S p CG-P s" , " CG-S rt CG-R s" , %r1;" CG-S CG-LINE
   SB-RESET s" @" CG-S p CG-P s"  red.global.add.f32 [" CG-S a CG-RD s" ], " CG-S tile CG-F s" ;" CG-S CG-LINE ;

\ --- RMSNorm scale ops -----------------------------------------------------------
\ UN : the ABI k register (row length n) converted to an f32 uniform, the mean
\ denominator (mean = sum/n). Every lane holds the same value, so it is uniform.
: EMIT-UN ( -- n )
   CG-NEXT-F {: r:n :}
   SB-RESET s" cvt.rn.f32.u32 " CG-S r CG-F s" , %r1;" CG-S CG-LINE
   r ;

\ USQRT : block-uniform sqrt.rn (correctly rounded); r = sqrt(u).
: EMIT-USQRT ( n -- n ) {: u:n :}
   CG-NEXT-F {: r:n :}
   SB-RESET s" sqrt.rn.f32 " CG-S r CG-F s" , " CG-S u CG-F s" ;" CG-S CG-LINE
   r ;

\ RMS-EPS+ : add the RMSNorm epsilon 1e-5 (0f3727C5AC = F64>F32(1e-5), the exact
\ f32 nearest maki/rmsnorm.f RMS-EPS) to the mean-square uniform BEFORE the sqrt.
: EMIT-RMS-EPS+ ( n -- n ) {: u:n :}
   CG-NEXT-F {: r:n :}
   SB-RESET s" add.f32 " CG-S r CG-F s" , " CG-S u CG-F s" , 0f3727C5AC;" CG-S CG-LINE
   r ;

\ --- RoPE pair rotation ----------------------------------------------------------
\ EMIT-ROPE-CORE : lane i, partner p=i^1 (its rotary-pair sibling, always in-warp).
\ partner = shfl.bfly(x,1) reads p's coordinate; then xc = x*cos, ps = partner*sin,
\ and the result selects by lane parity: even lane -> xc - ps, odd lane -> xc + ps
\ (forward). The backward (bwd<>0) is the rotation by the negative angle, i.e. the
\ SAME instructions with the even/odd branches swapped (the pair sign flipped).
\ shfl.bfly is warp-scoped (no bar.sync); the -1 membermask is well-formed because
\ the straight-line kernel body reaches it uniformly across the warp.
: EMIT-ROPE-CORE ( n n n n -- n ) {: x:n cs:n sn:n bwd:n :}
   CG-NEXT-F {: p:n :}
   SB-RESET s" shfl.sync.bfly.b32 " CG-S p CG-F s" , " CG-S x CG-F s" , 1, 0x1f, -1;" CG-S CG-LINE
   CG-NEXT-R {: rt:n :}
   SB-RESET s" mov.u32 " CG-S rt CG-R s" , %tid.x;" CG-S CG-LINE
   CG-NEXT-R {: ro:n :}
   SB-RESET s" and.b32 " CG-S ro CG-R s" , " CG-S rt CG-R s" , 1;" CG-S CG-LINE
   CG-NEXT-P {: podd:n :}
   SB-RESET s" setp.eq.u32 " CG-S podd CG-P s" , " CG-S ro CG-R s" , 1;" CG-S CG-LINE
   CG-NEXT-F {: xc:n :}
   SB-RESET s" mul.f32 " CG-S xc CG-F s" , " CG-S x CG-F s" , " CG-S cs CG-F s" ;" CG-S CG-LINE
   CG-NEXT-F {: ps:n :}
   SB-RESET s" mul.f32 " CG-S ps CG-F s" , " CG-S p CG-F s" , " CG-S sn CG-F s" ;" CG-S CG-LINE
   CG-NEXT-F {: fa:n :}
   SB-RESET s" add.f32 " CG-S fa CG-F s" , " CG-S xc CG-F s" , " CG-S ps CG-F s" ;" CG-S CG-LINE
   CG-NEXT-F {: fs:n :}
   SB-RESET s" sub.f32 " CG-S fs CG-F s" , " CG-S xc CG-F s" , " CG-S ps CG-F s" ;" CG-S CG-LINE
   CG-NEXT-F {: out:n :}
   bwd 0= if
      SB-RESET s" selp.f32 " CG-S out CG-F s" , " CG-S fa CG-F s" , " CG-S fs CG-F s" , " CG-S podd CG-P s" ;" CG-S CG-LINE
   else
      SB-RESET s" selp.f32 " CG-S out CG-F s" , " CG-S fs CG-F s" , " CG-S fa CG-F s" , " CG-S podd CG-P s" ;" CG-S CG-LINE
   then
   out ;

: EMIT-ROPE-ROT ( n n n -- n )      0 EMIT-ROPE-CORE ;
: EMIT-ROPE-ROT-BWD ( n n n -- n )  1 EMIT-ROPE-CORE ;
