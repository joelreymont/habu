\ cg-attention.f - PTX codegen: a FUSED attention kernel (the flash-attention seed).

require lib/errors.f
require lib/string.f
require lib/float.f
require lib/fmt.f
require src/arch/ptx/emit.f
require lib/ptx/cg.f
require lib/ptx/header.f
\
\ ============================ DESIGN NOTES (read me) ============================
\
\ WHAT: O = softmax(Q*K^T) * V  for one head. EMIT-ATTN emits a self-contained
\ kernel `ATTN`. It is the composition of the GEMM (cg-matmul.f) and the fused
\ softmax (cg-collective.f) - and it is FUSED: the N x N attention-score matrix
\ lives only in shared memory and is NEVER written to global. That non-
\ materialization of the scores is exactly the win flash-attention exists for, and
\ here it falls out of writing the three phases in one kernel body.
\
\ SCHEME (one query per block; block = N threads; head dim D):
\   q = ctaid.x (the query row), tid = thread.
\   Phase 0  stage Qs[tid] = Q[q][tid]            (tid<D), bar.sync
\   Phase 1  S[tid] = sum_k Qs[k]*K[tid][k]       (tid<N: this thread's score), bar.sync
\            -> S[] is the q-th ROW of Q*K^T, held in SHARED (never global)
\   Phase 2  thread 0 row-softmaxes S in place: m=max S; S[i]=exp(S[i]-m); l=sum S
\            (exp via ex2.approx(x*log2e), the same trick as SOFTMAX-ROWS), bar.sync
\   Phase 3  O[q][tid] = (1/l) * sum_i S[i]*V[i][tid]   (tid<D), store
\
\ CORRECTNESS: device output matches a CPU reference attention to max|err| ~1.8e-7
\ (N=128, D=64) - within fp32 epsilon.
\
\ PERFORMANCE (Orin NX): ~8 GFLOP/s at N=128,D=64 - a CORRECTNESS BASELINE, not a
\ tuned number. It is slow on purpose-of-simplicity: one query/block, the softmax is
\ SERIAL in thread 0, and phase-3 leaves N-D threads idle. The optimized flash path
\ (dotted habu-ptx-m11-attention) keeps the SAME fused structure but: tiles queries
\ (BM>1/block) + reuses the register-blocked GEMM (cg-matmul.f) for Q*K^T and P*V;
\ does a PARALLEL block-reduction softmax (cg-collective.f) instead of serial; and
\ uses the ONLINE softmax (running max/sum rescaling) so N need not fit in shared.
\ Every one of those is code we already have or can emit - no fundamental barrier,
\ same story as the GEMM.
\
\ LIMITS: shared layout is fixed for N<=128, D<=64 (Qs[64]@0, S[128]@256, l@768);
\ runtime pN,pD must be within that. CHECKED SURFACE: ATTN:CHECKED relates all four
\ [N,D] matrices and threads a phase-indexed context plus register accumulator through
\ stage-Q, score, softmax, and output. Reordering or omitting a phase is a type error.
\ The PTX instruction chunks remain trusted target primitives, as with LOAD/STORE/FMA.
\ Load after src/arch/ptx/emit.f + lib/ptx/cg.f.
\ ==============================================================================

package ATTN

private

64 constant D-MAX   128 constant N-MAX
D-MAX 4 *         constant S-OFF     \ byte offset of S[] in shared = 256
D-MAX N-MAX + 4 * constant L-OFF     \ byte offset of l in shared    = 768

: OFF ( ptr u8 n n -- ) {: ap:ptr au:n v:n :}     \ emit "<a><v>;"
   SB-RESET  ap au SB-APPEND  v SB-U  s" ;" SB-APPEND  SB$ PTX-L ;

: SETUP-EMIT ( -- )
   s" ld.param.u64 %rd1,[pQ];" PTX-L  s" ld.param.u64 %rd2,[pK];" PTX-L  s" ld.param.u64 %rd3,[pV];" PTX-L  s" ld.param.u64 %rd4,[pO];" PTX-L
   s" ld.param.u32 %r1,[pN];" PTX-L  s" ld.param.u32 %r2,[pD];" PTX-L
   s" cvta.to.global.u64 %rd1,%rd1;" PTX-L  s" cvta.to.global.u64 %rd2,%rd2;" PTX-L
   s" cvta.to.global.u64 %rd3,%rd3;" PTX-L  s" cvta.to.global.u64 %rd4,%rd4;" PTX-L
   s" mov.u32 %r3,%ctaid.x;" PTX-L  s" mov.u32 %r4,%tid.x;" PTX-L  s" mov.u32 %r5,SH;" PTX-L
   s" mad.lo.u32 %r6,%r3,%r2,%r4;" PTX-L ;                        \ q*D+tid

: STAGE-Q-EMIT ( -- )
   s" setp.ge.u32 %p1,%r4,%r2;" PTX-L  s" @%p1 bra $SK0;" PTX-L
   s" mul.wide.u32 %rd10,%r6,4;" PTX-L  s" add.u64 %rd10,%rd1,%rd10;" PTX-L  s" ld.global.f32 %f2,[%rd10];" PTX-L
   s" shl.b32 %r7,%r4,2;" PTX-L  s" add.u32 %r7,%r5,%r7;" PTX-L  s" st.shared.f32 [%r7],%f2;" PTX-L  s" $SK0:" PTX-L  s" bar.sync 0;" PTX-L ;

: SCORE-EMIT ( -- )
   s" setp.ge.u32 %p1,%r4,%r1;" PTX-L  s" @%p1 bra $SK1;" PTX-L
   s" mul.lo.u32 %r8,%r4,%r2;" PTX-L  s" mov.f32 %f1,0f00000000;" PTX-L  s" mov.u32 %r9,0;" PTX-L
   s" $L1:" PTX-L  s" setp.ge.u32 %p2,%r9,%r2;" PTX-L  s" @%p2 bra $E1;" PTX-L
   s" shl.b32 %r10,%r9,2;" PTX-L  s" add.u32 %r10,%r5,%r10;" PTX-L  s" ld.shared.f32 %f3,[%r10];" PTX-L
   s" add.u32 %r11,%r8,%r9;" PTX-L  s" mul.wide.u32 %rd11,%r11,4;" PTX-L  s" add.u64 %rd11,%rd2,%rd11;" PTX-L  s" ld.global.f32 %f4,[%rd11];" PTX-L
   s" fma.rn.f32 %f1,%f3,%f4,%f1;" PTX-L  s" add.u32 %r9,%r9,1;" PTX-L  s" bra $L1;" PTX-L  s" $E1:" PTX-L
   s" shl.b32 %r12,%r4,2;" PTX-L  s" add.u32 %r12,%r5,%r12;" PTX-L  s" add.u32 %r12,%r12," S-OFF OFF
   s" st.shared.f32 [%r12],%f1;" PTX-L  s" $SK1:" PTX-L  s" bar.sync 0;" PTX-L ;

: SOFTMAX-EMIT ( -- )
   s" setp.ne.u32 %p1,%r4,0;" PTX-L  s" @%p1 bra $SK2;" PTX-L
   s" mov.u32 %r13,%r5;" PTX-L  s" add.u32 %r13,%r13," S-OFF OFF  s" mov.f32 %f5,0fFF800000;" PTX-L  s" mov.u32 %r14,0;" PTX-L
   s" $L2:" PTX-L  s" setp.ge.u32 %p2,%r14,%r1;" PTX-L  s" @%p2 bra $E2;" PTX-L
   s" shl.b32 %r15,%r14,2;" PTX-L  s" add.u32 %r15,%r13,%r15;" PTX-L  s" ld.shared.f32 %f6,[%r15];" PTX-L  s" max.f32 %f5,%f5,%f6;" PTX-L
   s" add.u32 %r14,%r14,1;" PTX-L  s" bra $L2;" PTX-L  s" $E2:" PTX-L
   s" mov.f32 %f7,0f00000000;" PTX-L  s" mov.u32 %r14,0;" PTX-L
   s" $L3:" PTX-L  s" setp.ge.u32 %p2,%r14,%r1;" PTX-L  s" @%p2 bra $E3;" PTX-L
   s" shl.b32 %r15,%r14,2;" PTX-L  s" add.u32 %r15,%r13,%r15;" PTX-L  s" ld.shared.f32 %f6,[%r15];" PTX-L
   s" sub.f32 %f6,%f6,%f5;" PTX-L  s" mul.f32 %f6,%f6,0f3FB8AA3B;" PTX-L  s" ex2.approx.f32 %f6,%f6;" PTX-L
   s" st.shared.f32 [%r15],%f6;" PTX-L  s" add.f32 %f7,%f7,%f6;" PTX-L  s" add.u32 %r14,%r14,1;" PTX-L  s" bra $L3;" PTX-L  s" $E3:" PTX-L
   s" mov.u32 %r16,%r5;" PTX-L  s" add.u32 %r16,%r16," L-OFF OFF  s" st.shared.f32 [%r16],%f7;" PTX-L  s" $SK2:" PTX-L  s" bar.sync 0;" PTX-L ;

: OUTPUT-EMIT ( -- )
   s" setp.ge.u32 %p1,%r4,%r2;" PTX-L  s" @%p1 bra $SK3;" PTX-L
   s" mov.u32 %r13,%r5;" PTX-L  s" add.u32 %r13,%r13," S-OFF OFF  s" mov.f32 %f1,0f00000000;" PTX-L  s" mov.u32 %r14,0;" PTX-L
   s" $L4:" PTX-L  s" setp.ge.u32 %p2,%r14,%r1;" PTX-L  s" @%p2 bra $E4;" PTX-L
   s" shl.b32 %r15,%r14,2;" PTX-L  s" add.u32 %r15,%r13,%r15;" PTX-L  s" ld.shared.f32 %f8,[%r15];" PTX-L
   s" mad.lo.u32 %r17,%r14,%r2,%r4;" PTX-L  s" mul.wide.u32 %rd12,%r17,4;" PTX-L  s" add.u64 %rd12,%rd3,%rd12;" PTX-L  s" ld.global.f32 %f9,[%rd12];" PTX-L
   s" fma.rn.f32 %f1,%f8,%f9,%f1;" PTX-L  s" add.u32 %r14,%r14,1;" PTX-L  s" bra $L4;" PTX-L  s" $E4:" PTX-L
   s" mov.u32 %r16,%r5;" PTX-L  s" add.u32 %r16,%r16," L-OFF OFF  s" ld.shared.f32 %f7,[%r16];" PTX-L  s" div.rn.f32 %f1,%f1,%f7;" PTX-L
   s" mul.wide.u32 %rd13,%r6,4;" PTX-L  s" add.u64 %rd13,%rd4,%rd13;" PTX-L  s" st.global.f32 [%rd13],%f1;" PTX-L  s" $SK3:" PTX-L ;

TRUSTED: Q-REG ( n -- matrix<space-global,f32,extent-q,extent-d> ) ;
TRUSTED: K-REG ( n -- matrix<space-global,f32,extent-q,extent-d> ) ;
TRUSTED: V-REG ( n -- matrix<space-global,f32,extent-q,extent-d> ) ;
TRUSTED: O-REG ( n -- matrix<space-global,f32,extent-q,extent-d> ) ;

TRUSTED: STATE ( matrix<space-global,f32,q,d> matrix<space-global,f32,q,d> matrix<space-global,f32,q,d> matrix<space-global,f32,q,d> -- attnctx<q,d,attn-stage-q> attnacc<f32,block-128,mask-live> )
   2drop 2drop 0 0 ;

public

TRUSTED: STAGE-Q ( attnctx<q,d,attn-stage-q> attnacc<f32,b,m> -- attnctx<q,d,attn-stage-score> attnacc<f32,b,m> )
   STAGE-Q-EMIT ;

TRUSTED: SCORE ( attnctx<q,d,attn-stage-score> attnacc<f32,b,m> -- attnctx<q,d,attn-stage-softmax> attnacc<f32,b,m> )
   SCORE-EMIT ;

TRUSTED: SOFTMAX ( attnctx<q,d,attn-stage-softmax> attnacc<f32,b,m> -- attnctx<q,d,attn-stage-output> attnacc<f32,b,m> )
   SOFTMAX-EMIT ;

TRUSTED: OUTPUT ( attnctx<q,d,attn-stage-output> attnacc<f32,b,m> -- attnctx<q,d,attn-stage-done> attnacc<f32,b,m> )
   OUTPUT-EMIT ;

: START ( matrix<space-global,f32,q,d> matrix<space-global,f32,q,d> matrix<space-global,f32,q,d> matrix<space-global,f32,q,d> -- attnctx<q,d,attn-stage-q> attnacc<f32,block-128,mask-live> )
   SETUP-EMIT
   STATE ;

: FINISH ( attnctx<q,d,attn-stage-done> attnacc<f32,b,m> -- )
   2drop ;

KERNEL: CHECKED ( matrix<space-global,f32,extent-q,extent-d> matrix<space-global,f32,extent-q,extent-d> matrix<space-global,f32,extent-q,extent-d> matrix<space-global,f32,extent-q,extent-d> -- )  GRID: extent-q
   START STAGE-Q SCORE SOFTMAX OUTPUT FINISH ;

: EMIT ( -- )
   PTX-HEADER-SM87  PTX-NL
   s" .visible .entry ATTN(.param .u64 pQ,.param .u64 pK,.param .u64 pV,.param .u64 pO,.param .u32 pN,.param .u32 pD)" PTX-L
   s" {" PTX-L
   s" .reg .pred %p<4>;" PTX-L  s" .reg .f32 %f<16>;" PTX-L  s" .reg .b32 %r<32>;" PTX-L  s" .reg .b64 %rd<24>;" PTX-L
   SB-RESET s" .shared .align 4 .b8 SH[" SB-APPEND L-OFF 8 + SB-U s" ];" SB-APPEND SB$ PTX-L
   1 Q-REG  2 K-REG  3 V-REG  4 O-REG  CHECKED
   s" ret;" PTX-L  s" }" PTX-L ;

;package
