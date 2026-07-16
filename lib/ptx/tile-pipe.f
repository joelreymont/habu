\ tile-pipe.f - typed pipelined register-blocked GEMM tile vocabulary (stage 1).
\
\ Closes the four capability gaps of habu-typed-pipelined-register-4d20acb5: the
\ checked surface for the cp.async double-buffered, register-blocked, vectorized
\ SGEMM that lib/ptx/cg-matmul.f emits. Every body REUSES the proven MM-* emitters
\ verbatim, so a kernel composed from these words lowers BYTE-IDENTICALLY to the
\ device-proven EMIT-MATMUL golden (pinned by lib/ptx/tile-pipe-test.f) - the
\ tile-v4a byte-identity discipline.
\
\ Nominal families (src/core/type-family.f) and the invariants they state
\ fail-closed TODAY (negatives in lib/ptx/tile-pipe-neg-test.f):
\
\   mmstage<t,b,l,w,p>   the READY current-parity staged As+Bs tile-pair. Minted
\                        ONLY by PIPE-LOOP per pipeline iteration (after the
\                        emitted wait_group + bar.sync); nominally distinct from
\                        span/tile, so every scalar/naive path (STAGE, SLOAD,
\                        STORE) rejects on it.
\   mmaslice<t,b,l,w,p>  the thread's strided A micro-tile slice (row stride
\                        32f = 128B): scalar loads only - it is NOT a legal v4
\                        base, and B-FRAG.V4 rejects it.
\   mmbslice<t,b,l,w,p>  the thread's contiguous B slice. The layout proof
\                        (SH .align 16, +MM-ASB, +tx*16) makes its base 16B
\                        aligned; like tile-v4a's vspan, this family IS the
\                        alignment obligation, and STAGE-SLICES is the only
\                        constructor. B-FRAG.V4 demands it.
\   mmafrag<t,b,l,w,p> / mmbfrag<t,b,l,w,p>
\                        one k-column A / k-row B operand fragment (4 regs).
\                        Distinct families keep the FMA operand ROLES straight
\                        (swapped A/B would compute C^T - rejected).
\   mmracc<t,b,g,w>      the register-blocked micro-tile accumulator (16 regs,
\                        g = geom-mt4x4). Distinct from acc<> and tile<>: a naive
\                        STORE on it rejects; only PIPE-STORE finalizes it.
\
\ Type parameters carry the relational facts: l = the blocked 2-D layout atom
\ geom-as64x32-bs32x64 (As[64][32] + Bs[32][64], row-major, strides 32/64 floats,
\ double-buffered, .align 16) pinned by every consumer word, so operands from a
\ different layout can never mix; p = the buffer-parity variable - PIPE-LOOP's
\ body is universally quantified over p, so a body cannot assume a specific
\ buffer, and RB-FMA requires both fragments to carry THE SAME p (no cross-buffer
\ operand mixing); w = the mask token threading acc and fragments.
\
\ TRUSTED boundary (owner habu-checker-cp-async-6ba788a5, scheduled for deletion):
\ the DYNAMIC pipeline protocol - cp.async issue -> commit_group -> wait_group N
\ -> bar.sync -> read, with pending/ready ordering and loop-carried double-buffer
\ parity ALTERNATION - is beyond the current type system, so the bodies below are
\ named trusted boundaries over the device-proven emitters. When the cp.async
\ typestate capability lands, these bodies re-certify as checked code and the
\ TRUSTED.md rows are removed. Load after lib/ptx/cg-matmul-emit.f (the raw
\ emitters; the checked production kernel in lib/ptx/cg-matmul.f sits ABOVE
\ this vocabulary).

require lib/ptx/cg-matmul-emit.f

variable PIPE-XT   \ pipeline compute-slot body xt (set by PIPE-LOOP, run by PIPE-RUN)

\ compute-slot adapter: MM-PIPE-KLOOP-WITH executes it once, in the slot where the
\ staged tile is READY (post wait_group + bar.sync); it mints the current-iteration
\ mmstage token, threads the mmracc token through the body, and discards the
\ resulting token (the accumulator registers ARE the carried state).
TRUSTED: PIPE-RUN ( -- )
   0 0 PIPE-XT @ execute drop ;

\ tile/thread coordinate derivation (ctaid/tid decomposition, shared base) + the
\ typed A[M,K] * B[K,N] -> C[M,N] contract, consuming the three ABI matrices.
TRUSTED: PIPE-SETUP ( matrix<space-global,f32,m,k> matrix<space-global,f32,k,q> matrix<space-global,f32,m,q> -- mmctx<m,k,q> )
   drop drop drop MM-THREAD-SETUP 0 ;

\ the zeroed 16-register 4x4 micro-tile accumulator (%f10..%f25)
TRUSTED: PIPE-ACC-ZERO ( mmctx<m,k,q> -- mmctx<m,k,q> mmracc<f32,block-256,geom-mt4x4,mask-live> )
   MM-ACC-ZERO-EMIT 10 ;

\ capability (1): the cp.async double-buffered software pipeline (prologue stage,
\ runtime $KLOOP, prefetch-other-buffer, commit_group/wait_group, barriers, parity
\ flip). The body sees one READY stage per iteration and must preserve the
\ accumulator; p is universally quantified, so the body cannot assume a parity.
TRUSTED: PIPE-LOOP ( mmctx<m,k,q> mmracc<f32,block-256,g,w> [ mmstage<f32,block-256,geom-as64x32-bs32x64,w,p> mmracc<f32,block-256,g,w> -- mmracc<f32,block-256,g,w> ] -- mmctx<m,k,q> mmracc<f32,block-256,g,w> )
   PIPE-XT !  [: PIPE-RUN ;] MM-PIPE-KLOOP-WITH ;

\ capability (4): the blocked 2-D layout, split into its two differently-shaped
\ slices - strided A (scalar-only) and contiguous 16B-aligned B (v4-legal)
TRUSTED: STAGE-SLICES ( mmstage<f32,block-256,geom-as64x32-bs32x64,w,p> -- mmaslice<f32,block-256,geom-as64x32-bs32x64,w,p> mmbslice<f32,block-256,geom-as64x32-bs32x64,w,p> )
   drop MM-CUR-BASES 12 13 ;

\ one k-column of A: 4 strided scalar ld.shared (%f26..29)
TRUSTED: A-FRAG ( mmaslice<f32,block-256,geom-as64x32-bs32x64,w,p> n -- mmafrag<f32,block-256,geom-as64x32-bs32x64,w,p> )
   nip MM-KSTEP-A 26 ;

\ capability (3): the vectorized shared load - one ld.shared.v4.f32 (%f30..33).
\ Demands the 16B-alignment-proven contiguous mmbslice; a strided mmaslice or a
\ plain shared span is a type error.
TRUSTED: B-FRAG.V4 ( mmbslice<f32,block-256,geom-as64x32-bs32x64,w,p> n -- mmbfrag<f32,block-256,geom-as64x32-bs32x64,w,p> )
   nip MM-KSTEP-B 30 ;

\ capability (2): the register-blocked outer product - 16 FMAs reusing the 4+4
\ loaded operands. Fragment roles are nominal (A-major rows, B-major cols) and
\ both fragments must carry the same parity p and mask w as the accumulator.
TRUSTED: RB-FMA ( mmracc<f32,block-256,geom-mt4x4,w> mmafrag<f32,block-256,geom-as64x32-bs32x64,w,p> mmbfrag<f32,block-256,geom-as64x32-bs32x64,w,p> -- mmracc<f32,block-256,geom-mt4x4,w> )
   2drop MM-KSTEP-FMA ;

\ the CHECKED emit-time k-unroll: applies an accumulator-preserving indexed body
\ cnt times, re-supplying the loop-invariant slices. Certifies with the existing
\ typed-quotation machinery - not a trusted boundary.
: K-UNROLL ( n mmaslice<t,b,l,w,p> mmbslice<t,b,l,w,p> mmracc<t,b,g,w> [ mmracc<t,b,g,w> n mmaslice<t,b,l,w,p> mmbslice<t,b,l,w,p> -- mmracc<t,b,g,w> ] -- mmracc<t,b,g,w> )
   {: cnt:n as bs acc body :} \ typed-local-lint: allow-bare-local - parametric tile-pipe types contain commas.
   acc
   cnt 0 ?do  i as bs body execute  loop ;

\ finalize: write the 4x4 micro-tile of C (16 st.global), consuming ctx + acc
TRUSTED: PIPE-STORE ( mmctx<m,k,q> mmracc<f32,block-256,geom-mt4x4,mask-live> -- )
   MM-WRITE 2drop ;
