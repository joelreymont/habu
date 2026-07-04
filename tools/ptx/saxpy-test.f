\ saxpy-test.f - focused tests for PTX text and checked-codegen output.
\
\ Load after lib/ptx/test-prelude.f and lib/ptx/process-test-prelude.f.

require lib/ptx/test-prelude.f
require lib/ptx/process-test-prelude.f

$8000 constant PTXT-CAP
10000 constant PTXT-TIMEOUT-MS

create PTXT-OUT PTXT-CAP allot
create PTXT-ERR PTXT-CAP allot

variable PTXT-OUT-U
variable PTXT-OUT-SAVE
variable PTXT-ERR-SAVE

0 constant PTXT-F-DUPFD
10 constant PTXT-FD-SAVE-MIN
1 constant PTXT-STDOUT-FD
2 constant PTXT-STDERR-FD

: PTXT-CAPTURE>N ( len len rc -- n n n ) {: outu erru rc :}
   outu LEN>N erru LEN>N rc RC>N ;

: PTXT-DUP-FD ( n -- n ) {: fd:n :}
   fd PTXT-F-DUPFD PTXT-FD-SAVE-MIN fcntl dup 0 < if E-PROC-OUTPUT throw then ;

: PTXT-DUP2! ( n n -- ) {: src:n dst:n :}
   src dst dup2 dup 0 < if drop E-PROC-OUTPUT throw then drop ;

: PTXT-REDIRECT! ( -- )
   PTXT-STDOUT-FD PTXT-DUP-FD PTXT-OUT-SAVE !
   PTXT-STDERR-FD PTXT-DUP-FD PTXT-ERR-SAVE !
   PROC-OUT-W @ FD>N PTXT-STDOUT-FD PTXT-DUP2!
   PROC-ERR-W @ FD>N PTXT-STDERR-FD PTXT-DUP2!
   PROC-OUT-W PROC-CLOSE-CELL
   PROC-ERR-W PROC-CLOSE-CELL ;

: PTXT-RESTORE! ( -- )
   PTXT-OUT-SAVE @ PTXT-STDOUT-FD PTXT-DUP2!
   PTXT-ERR-SAVE @ PTXT-STDERR-FD PTXT-DUP2!
   PTXT-OUT-SAVE @ close
   PTXT-ERR-SAVE @ close
   -1 PTXT-OUT-SAVE !
   -1 PTXT-ERR-SAVE ! ;

: PTXT-DRAIN ( -- )
   PTXT-OUT PTXT-CAP >LEN PTXT-ERR PTXT-CAP >LEN PROC-RUN-CAPTURE-LOOP ;

: PTXT-STORE-RC ( n -- ) {: rc:n :}
   rc >RC PROC-RC ! ;

\ typed-local-lint: allow-bare-local - q is the action quotation under capture.
: PTXT-CAPTURE ( [ -- ] -- n n n ) {: q :}
   PTXT-TIMEOUT-MS >MS PROC-CAPTURE-BEGIN
   PTXT-REDIRECT!
   q catch {: rc:n :}
   PTXT-RESTORE!
   PTXT-DRAIN
   PROC-CLOSE-ALL-CAPTURE-FDS
   rc PTXT-STORE-RC
   PROC-CAPTURE-RC@ PTXT-CAPTURE>N ;

: PTXT-HAS ( ptr u8 n -- ) {: a:ptr u :}
   PTXT-OUT PTXT-OUT-U @ a u CONTAINS? TTRUE ;

: PTXT-NOT-HAS ( ptr u8 n -- ) {: a:ptr u :}
   PTXT-OUT PTXT-OUT-U @ a u CONTAINS? 0= TTRUE ;

\ non-overlapping occurrences of b/v in a/u (the text-count the .version-once
\ regression needs; CONTAINS? only proves presence, not multiplicity).
: PTXT-SUB-COUNT ( ptr u8 n ptr u8 n -- n ) {: a:ptr u:n b:ptr v:n :}
   a u b v FIND-SUB {: i:n :}
   i 0 < if 0 exit then
   i v + {: adv:n :}
   a adv + u adv - b v RECURSE 1+ ;

: PTXT-COUNT ( ptr u8 n -- n ) {: b:ptr v:n :}   \ occurrences of b/v in the captured stream
   PTXT-OUT PTXT-OUT-U @ b v PTXT-SUB-COUNT ;

: PTXT-INCLUDE-SAXPY ( -- )
   s" tools/ptx/saxpy.f" included ;

: PTXT-INCLUDE-OPS-CG ( -- )
   s" tools/ptx/ops-cg.f" included ;

: PTXT-INCLUDE-ONCE-CG ( -- )
   s" tools/ptx/once-cg.f" included ;

: PTXT-INCLUDE-SOFTMAX-CG ( -- )
   s" tools/ptx/softmax-cg.f" included ;

: PTXT-INCLUDE-SUM-CG ( -- )
   s" tools/ptx/sum-cg.f" included ;

: PTXT-INCLUDE-SUM1024-CG ( -- )
   s" tools/ptx/sum1024-cg.f" included ;

: PTXT-INCLUDE-MATMUL-CG ( -- )
   s" tools/ptx/matmul-cg.f" included ;

: PTXT-INCLUDE-SMEM-CG ( -- )
   s" tools/ptx/smem-cg.f" included ;

: PTXT-INCLUDE-SOFTMAX-BWD-CG ( -- )
   s" tools/ptx/softmax-bwd-cg.f" included ;

: PTXT-INCLUDE-SOFTMAX-BWD-OPT-CG ( -- )
   s" tools/ptx/softmax-bwd-opt-cg.f" included ;

: PTXT-RUN-SAXPY ( -- n n n )
   [: PTXT-INCLUDE-SAXPY ;] PTXT-CAPTURE ;

: PTXT-RUN-OPS-CG ( -- n n n )
   [: PTXT-INCLUDE-OPS-CG ;] PTXT-CAPTURE ;

: PTXT-RUN-ONCE-CG ( -- n n n )
   [: PTXT-INCLUDE-ONCE-CG ;] PTXT-CAPTURE ;

: PTXT-RUN-SOFTMAX-CG ( -- n n n )
   [: PTXT-INCLUDE-SOFTMAX-CG ;] PTXT-CAPTURE ;

: PTXT-RUN-SUM-CG ( -- n n n )
   [: PTXT-INCLUDE-SUM-CG ;] PTXT-CAPTURE ;

: PTXT-RUN-SUM1024-CG ( -- n n n )
   [: PTXT-INCLUDE-SUM1024-CG ;] PTXT-CAPTURE ;

: PTXT-RUN-MATMUL-CG ( -- n n n )
   [: PTXT-INCLUDE-MATMUL-CG ;] PTXT-CAPTURE ;

: PTXT-RUN-SMEM-CG ( -- n n n )
   [: PTXT-INCLUDE-SMEM-CG ;] PTXT-CAPTURE ;

: PTXT-RUN-SOFTMAX-BWD-CG ( -- n n n )
   [: PTXT-INCLUDE-SOFTMAX-BWD-CG ;] PTXT-CAPTURE ;

: PTXT-RUN-SOFTMAX-BWD-OPT-CG ( -- n n n )
   [: PTXT-INCLUDE-SOFTMAX-BWD-OPT-CG ;] PTXT-CAPTURE ;

\ The combined fwd+bwd driver INCLUDES softmax-cg.f, which redefines SOFTMAX-ROWS;
\ that collides with this suite's own PTXT-SOFTMAX-CG-OUTPUT include. So emit it in a
\ CHILD process (fresh dictionary) - the same self-emit path softmax-gradcheck uses.
: PTXT-RUN-SOFTMAX-FB-SPAWN ( -- n n n )
   PROC-ARGV-RESET
   s" --load"                    >LEN PROC-ARGV+
   s" tools/ptx/softmax-fb-cg.f" >LEN PROC-ARGV+
   s" bin/hb" >LEN  PTXT-OUT PTXT-CAP >LEN  PTXT-ERR PTXT-CAP >LEN  PTXT-TIMEOUT-MS >MS  RUN-ARGV-CAPTURE
   PTXT-CAPTURE>N ;

: PTXT-SAXPY-OUTPUT ( -- )
   PTXT-RUN-SAXPY 0 T= 0 T= dup PTXT-OUT-U ! 0 > TTRUE
   s" .version 8.3" PTXT-HAS
   s" .target sm_87" PTXT-HAS
   s" .visible .entry SAXPY" PTXT-HAS
   s" ld.param.u64 %rd1, [p_x];" PTXT-HAS
   s" mad.lo.u32 %r5, %r2, %r3, %r4;" PTXT-HAS
   s" setp.ge.u32 %p1, %r5, %r1;" PTXT-HAS
   s" mul.rn.f32 %f4, %f1, %f2;" PTXT-HAS
   s" add.rn.f32 %f4, %f4, %f3;" PTXT-HAS
   s" st.global.f32 [%rd5], %f4;" PTXT-HAS
   s" DONE:" PTXT-HAS
   s" ERROR" PTXT-NOT-HAS ;

: PTXT-OPS-CG-OUTPUT ( -- )
   PTXT-RUN-OPS-CG 0 T= 0 T= dup PTXT-OUT-U ! 0 > TTRUE
   s" sub.rn.f32 %f4, %f2, %f3;" PTXT-HAS
   s" div.rn.f32 %f6, %f4, %f5;" PTXT-HAS
   s" fma.rn.f32" PTXT-HAS
   s" sub.rn.f32 %f10, %f2, %f6;" PTXT-HAS
   s" red.global.add.f32" PTXT-HAS
   s" div.rn.f32 %f18, %f10, %f14;" PTXT-HAS
   s" ld.global.v4.f32 {%f2, %f3, %f4, %f5}, [%rd4];" PTXT-HAS
   s" @%p3 ld.global.f32 %f2, [%rd4];" PTXT-HAS
   s" @%p6 ld.global.f32 %f5, [%rd7];" PTXT-HAS
   s" st.global.v4.f32 [%rd19], {%f18, %f19, %f20, %f21};" PTXT-HAS
   s" @%p18 st.global.f32 [%rd19], %f18;" PTXT-HAS
   s" @%p21 st.global.f32 [%rd22], %f21;" PTXT-HAS
   s" .version" PTXT-COUNT 1 T=             \ one module header for both scalar + v4 entries
   s" .visible .entry" PTXT-COUNT 2 T=
   s" ERROR" PTXT-NOT-HAS ;

: PTXT-ONCE-CG-OUTPUT ( -- )
   PTXT-RUN-ONCE-CG 0 T= 0 T= dup PTXT-OUT-U ! 0 > TTRUE
   s" .visible .entry ONCE_SPAN" PTXT-HAS
   s" ld.global.f32" PTXT-HAS
   s" st.global.f32" PTXT-HAS
   s" red.global.add.f32" PTXT-NOT-HAS
   s" ERROR" PTXT-NOT-HAS ;

: PTXT-SOFTMAX-CG-OUTPUT ( -- )
   PTXT-RUN-SOFTMAX-CG 0 T= 0 T= dup PTXT-OUT-U ! 0 > TTRUE
   s" .shared .align 4 .b8 SMEM[1024];" PTXT-HAS
   s" mov.f32 %f2, 0fFF800000;" PTXT-HAS
   s" @%p2 mov.f32 %f2, %f1;" PTXT-HAS
   s" mov.f32 %f8, 0f00000000;" PTXT-HAS
   s" @%p5 mov.f32 %f8, %f7;" PTXT-HAS
   s" ERROR" PTXT-NOT-HAS ;

: PTXT-SUM-CG-OUTPUT ( -- )
   PTXT-RUN-SUM-CG 0 T= 0 T= dup PTXT-OUT-U ! 0 > TTRUE
   s" .visible .entry SUM_ROWS" PTXT-HAS
   s" .shared .align 4 .b8 SMEM[1024];" PTXT-HAS
   s" mov.f32 %f2, 0f00000000;" PTXT-HAS
   s" @%p2 mov.f32 %f2, %f1;" PTXT-HAS
   s" setp.ge.u32 %p4, %r10, 256;" PTXT-HAS
   s" add.f32 %f3, %f3, %f4;" PTXT-HAS
   s" .visible .entry SCATTER_ROWS" PTXT-HAS
   s" red.global.add.f32" PTXT-HAS
   s" .version" PTXT-COUNT 1 T=             \ EXACTLY one module header (the doubled-header regression)
   s" .visible .entry" PTXT-COUNT 2 T=      \ both kernels share that one module
   s" ERROR" PTXT-NOT-HAS ;

: PTXT-SUM1024-CG-OUTPUT ( -- )
   PTXT-RUN-SUM1024-CG 0 T= 0 T= dup PTXT-OUT-U ! 0 > TTRUE
   s" .visible .entry SUM_ROWS_1024" PTXT-HAS
   s" .shared .align 4 .b8 SMEM[4096];" PTXT-HAS
   s" mov.f32 %f2, 0f00000000;" PTXT-HAS
   s" @%p2 mov.f32 %f2, %f1;" PTXT-HAS
   s" setp.ge.u32 %p4, %r10, 1024;" PTXT-HAS
   s" add.f32 %f3, %f3, %f4;" PTXT-HAS
   s" ERROR" PTXT-NOT-HAS ;

: PTXT-MATMUL-CG-OUTPUT ( -- )
   PTXT-RUN-MATMUL-CG 0 T= 0 T= dup PTXT-OUT-U ! 0 > TTRUE
   s" .visible .entry MM" PTXT-HAS
   s" .shared .align 16 .b8 SH[32768];" PTXT-HAS   \ 2 buffers of As[64][32]+Bs[32][64] (cp.async double buffer)
   s" setp.ge.u32 %p1,%r14,%r3;" PTXT-HAS
   s" @%p1 bra $KEND;" PTXT-HAS
   s" bar.sync 0;" PTXT-HAS
   s" cp.async.cg.shared.global" PTXT-HAS          \ direct global->shared staging
   s" cp.async.commit_group;" PTXT-HAS
   s" cp.async.wait_group 1;" PTXT-HAS
   s" ld.shared.v4.f32 {%f30,%f31,%f32,%f33}," PTXT-HAS   \ vectorized 4-column B load
   s" fma.rn.f32 %f10,%f26,%f30,%f10;" PTXT-HAS
   s" st.global.f32 [%rd12],%f25;" PTXT-HAS
   s" @%p1 bra DONE" PTXT-NOT-HAS
   s" E-PTX-NOIMPL" PTXT-NOT-HAS
   s" ERROR" PTXT-NOT-HAS ;

: PTXT-SMEM-CG-OUTPUT ( -- )
   PTXT-RUN-SMEM-CG 0 T= 0 T= dup PTXT-OUT-U ! 0 > TTRUE
   s" .visible .entry SMEM_CHECKED" PTXT-HAS
   s" .shared .align 4 .b8 SMEM[1024];" PTXT-HAS
   s" mov.u32 %r2, %tid.x;" PTXT-HAS
   s" st.shared.f32" PTXT-HAS
   s" ld.shared.f32" PTXT-HAS
   s" bar.sync 0;" PTXT-HAS
   s" @%p1 bra DONE" PTXT-NOT-HAS
   s" E-PTX-NOIMPL" PTXT-NOT-HAS
   s" ERROR" PTXT-NOT-HAS ;

: PTXT-SOFTMAX-BWD-CG-OUTPUT ( -- )
   PTXT-RUN-SOFTMAX-BWD-CG 0 T= 0 T= dup PTXT-OUT-U ! 0 > TTRUE
   s" .visible .entry SOFTMAX_BWD" PTXT-HAS
   s" setp.eq.f32" PTXT-HAS
   s" div.rn.f32" PTXT-HAS
   s" E-PTX-NOIMPL" PTXT-NOT-HAS
   s" ERROR" PTXT-NOT-HAS ;

: PTXT-SOFTMAX-BWD-OPT-CG-OUTPUT ( -- )
   PTXT-RUN-SOFTMAX-BWD-OPT-CG 0 T= 0 T= dup PTXT-OUT-U ! 0 > TTRUE
   s" .visible .entry SOFTMAX_BWD_OPT" PTXT-HAS
   s" mul.rn.f32" PTXT-HAS
   s" add.f32" PTXT-HAS
   s" sub.f32" PTXT-HAS
   s" div.rn.f32" PTXT-NOT-HAS
   s" setp.eq.f32" PTXT-NOT-HAS
   s" E-PTX-NOIMPL" PTXT-NOT-HAS
   s" ERROR" PTXT-NOT-HAS ;

\ the combined fwd+bwd module softmax-gradcheck self-emits: ONE .version header for
\ BOTH the forward SOFTMAX_ROWS and the AD-derived SOFTMAX_BWD entries (the doubled-
\ header regression the Mac gate must catch, since macOS has no ptxas to reject it).
: PTXT-SOFTMAX-FB-CG-OUTPUT ( -- )
   PTXT-RUN-SOFTMAX-FB-SPAWN 0 T= 0 T= dup PTXT-OUT-U ! 0 > TTRUE
   s" .visible .entry SOFTMAX_ROWS" PTXT-HAS
   s" .visible .entry SOFTMAX_BWD" PTXT-HAS
   s" .version" PTXT-COUNT 1 T=             \ EXACTLY one module header for both entries
   s" .visible .entry" PTXT-COUNT 2 T=      \ fwd + bwd share that one module
   s" ERROR" PTXT-NOT-HAS ;

T-RESET
PTXT-SAXPY-OUTPUT
PTXT-OPS-CG-OUTPUT
PTXT-ONCE-CG-OUTPUT
PTXT-SOFTMAX-CG-OUTPUT
PTXT-SUM-CG-OUTPUT
PTXT-SUM1024-CG-OUTPUT
PTXT-MATMUL-CG-OUTPUT
PTXT-SMEM-CG-OUTPUT
PTXT-SOFTMAX-BWD-CG-OUTPUT
PTXT-SOFTMAX-BWD-OPT-CG-OUTPUT
PTXT-SOFTMAX-FB-CG-OUTPUT
T-REPORT
s" saxpy-test: ok" type cr
