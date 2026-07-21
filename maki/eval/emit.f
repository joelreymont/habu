\ maki/eval/emit.f - the OFF-DEVICE authoring autograder for the collective /
\ 2D-GEMM / attention tasks: GRADE = certify AND emit AND PTX structure.
\
\ Extends the SAXPY/softmax device-golden mechanism (maki/eval/device.f /
\ eval-device-sm.f) to three tasks whose device leg is Orin-gated: grading here
\ stops at the checker + a fresh child-process PTX emit + structural assertions
\ on the emitted PTX (required instruction features present, forbidden patterns
\ absent). The device-golden leg for these tasks is recorded as a SKIP by the
\ suites (device-FFI SKIP pattern, maki/device-smoke.f). Verdicts mirror
\ GRADE-CANDIDATE: 2 GREEN (certifies + emit + structure), 1 TYPED-WRONG
\ (certifies but a structural gate fails - e.g. sum-normalize
\ written with BLOCK-MAX, or a GEMM that skips the K-loop), 0 REJECTED.
\
\ Tasks and their structural gates (pinned from the committed emitters):
\ - sumnorm  out[r] = in[r]/sum(in[r])  (CG-SM scaffold, one block per row):
\   requires the shared-memory block reduction (bar.sync, st.shared.f32), the
\   add.f32 sum fold, div.rn.f32, st.global.f32; forbids max.f32 and
\   ex2.approx (a softmax-pattern-matched or max-reduced kernel certifies -
\   type-identical - but fails here).
\ - gemm     C = A*B (MM-AUTHOR scaffold, cg-matmul.f phase words): requires
\   the cp.async.cg.shared.global staged pipeline, the fma.rn.f32 compute,
\   bar.sync, st.global.f32; forbids st.shared.f32 (the pipeline stages
\   global->shared directly; a register round-trip is the wrong shape). A
\   candidate that skips MM-K-LOOP certifies but has no fma/cp.async.
\ - attention O = softmax(Q*K^T)*V (ATTN:AUTHOR scaffold, phase-token words):
\   requires ex2.approx.f32, st.shared.f32, bar.sync, st.global.f32; forbids
\   cp.async (lib/ptx/attention-checked-test.f pins the same). Phase tokens
\   make omission/reordering a checker reject, so 1 is unreachable by design.
\
\ Like the device graders, the candidate is compiled and emitted by a spawned
\ bin/hb child, never in-process: untrusted generated source must not mutate
\ this grader's dictionary. A failed emit child aborts grading with
\ E-EVAL-EMIT; infrastructure failure is never a candidate verdict. Load after
\ maki/eval/eval.f; this file owns its process setup.

require lib/errors.f
require lib/string.f
require lib/float.f
require lib/fmt.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/engine-candidate.f
require src/arch/ptx/emit.f
require lib/ptx/cg.f
require lib/ptx/header.f
require lib/ptx/cg-collective.f
require lib/ptx/collective.f
require lib/ptx/cg-matmul.f
require lib/ptx/cg-attention.f
require maki/eval/eval.f
require maki/device-artifacts.f

\ eval-emit reopens package EVAL as the off-device authoring-grade module.
\ CHECK-PASSES? is same-package (bare); the EE- driver/spawn/structure helpers
\ are private; GRADE-SUMNORM / GRADE-GEMM / GRADE-ATTN are the public surface.
package EVAL

private

$10000 constant EE-OUT-CAP
$1000 constant EE-ERR-CAP
20000 constant EE-TIMEOUT-MS
2 constant EE-STDERR-FD
create EE-OUT EE-OUT-CAP allot
create EE-ERR EE-ERR-CAP allot
variable EE-OUT-U
variable EE-ERR-U
PTR-VARIABLE EE-CAND-A
variable EE-CAND-U
variable EE-GRADE

defer EE-DRIVER$ ( -- ptr u8 n )

: EE-DRIVER-LIVE$ ( -- ptr u8 n )
   MAKI-GRADE:DRIVER$ ;

: EE-DRIVER-LIVE! ( -- )
   [: EE-DRIVER-LIVE$ ;] is EE-DRIVER$ ;

EE-DRIVER-LIVE!

\ ---- driver: %BLOCK line + candidate K + one scaffold line ----
: EE-WRITE-DRIVER ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: ba:ptr bu:n ca:ptr cu:n sa:ptr su:n :}
   SB-RESET
   ba bu SB-APPEND  STR-LF SB-APPEND-C
   s" : " SB-APPEND  ca cu SB-APPEND  s"  ;" SB-APPEND  STR-LF SB-APPEND-C
   sa su SB-APPEND  STR-LF SB-APPEND-C
   MAKI-GRADE:DRIVER$ SB$ WRITE-ALL ;

\ ---- spawn bin/hb over the shared PTX layers + task layers + the driver ----
: EE-ARGV-COMMON ( -- )
   PROC-ARGV-RESET
   s" --load"               >LEN PROC-ARGV+
   s" lib/errors.f"         >LEN PROC-ARGV+  s" lib/string.f" >LEN PROC-ARGV+
   s" lib/float.f"          >LEN PROC-ARGV+  s" lib/fmt.f"    >LEN PROC-ARGV+
   s" src/arch/ptx/emit.f"  >LEN PROC-ARGV+  s" lib/ptx/cg.f" >LEN PROC-ARGV+
   s" lib/ptx/header.f"     >LEN PROC-ARGV+ ;

: EE-ERR-WRITE ( n -- ) {: u:n :}
   u 0= if exit then
   EE-STDERR-FD EE-ERR u write u <> if E-EVAL-EMIT throw then ;

: EE-EMIT-FAIL ( n -- )
   EE-ERR-WRITE
   E-EVAL-EMIT throw ;

\ the signal is the captured PTX on stdout, exactly as in maki/eval/device.f
: EE-EMIT ( -- n )
   0 EE-OUT-U !
   0 EE-ERR-U !
   EE-DRIVER$              >LEN PROC-ARGV+
   ENGINE-CANDIDATE:PATH$ >LEN
   EE-OUT EE-OUT-CAP >LEN
   EE-ERR EE-ERR-CAP >LEN
   EE-TIMEOUT-MS >MS RUN-ARGV-CAPTURE
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE 0 >RC ENDOF          \ clean exit -> rc 0
     err OF PCAP-FAILED:UNMAKE ENDOF                  \ nonzero child: (out err code) on stack
   ;MATCH
   {: outu:len erru:len rc:rc :}
   erru LEN>N EE-ERR-U !
   rc RC>N 0 <> outu LEN>N 0= or if erru LEN>N EE-EMIT-FAIL then
   outu LEN>N EE-OUT-U !
   MAKI-GRADE:PTX$ EE-OUT outu LEN>N WRITE-ALL  outu LEN>N ;

: EE-PTX$ ( -- ptr u8 n )  EE-OUT EE-OUT-U @ ;

: EE-ERR$ ( -- ptr u8 n )  EE-ERR EE-ERR-U @ ;

: EE-HAS? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   EE-PTX$ a u CONTAINS? ;

\ ---- per-task structural gates (see the header for the rationale) ----
: SUMNORM-STRUCT? ( -- bool )
   s" bar.sync 0;" EE-HAS?
   s" st.shared.f32" EE-HAS? and
   s" add.f32" EE-HAS? and
   s" div.rn.f32" EE-HAS? and
   s" st.global.f32" EE-HAS? and
   s" max.f32" EE-HAS? 0= and
   s" ex2.approx" EE-HAS? 0= and ;

: GEMM-STRUCT? ( -- bool )
   s" cp.async.cg.shared.global" EE-HAS?
   s" fma.rn.f32" EE-HAS? and
   s" bar.sync 0;" EE-HAS? and
   s" st.global.f32" EE-HAS? and
   s" st.shared.f32" EE-HAS? 0= and ;

: ATTN-STRUCT? ( -- bool )
   s" ex2.approx.f32" EE-HAS?
   s" st.shared.f32" EE-HAS? and
   s" bar.sync 0;" EE-HAS? and
   s" st.global.f32" EE-HAS? and
   s" cp.async" EE-HAS? 0= and ;

: EE-VERDICT ( bool -- n )
   if 2 else 1 then ;

: EE-CLEAN-ACT ( -- )
   MAKI-GRADE:CLEAN ;

: EE-CLEAN-THROW ( n -- )
   {: rc:n :}
   [: EE-CLEAN-ACT ;] catch {: cleanrc:n :}
   \ Cleanup cannot propagate a second throw; preserve the primary grader error.
   rc 0 <> if rc throw then
   cleanrc 0 <> if cleanrc throw then ;

: EE-CAND! ( ptr u8 n -- )
   EE-CAND-U !
   EE-CAND-A ! ;

: EE-CAND$ ( -- ptr u8 n )
   EE-CAND-A @ EE-CAND-U @ ;

: EE-SUMNORM ( ptr u8 n -- n ) {: a:ptr u:n :}
   s" habu-grade-sumnorm" MAKI-GRADE:PREPARE
   s" 256 %BLOCK"  a u
   s" CG-SM-RESET CG-HEADER CG-SM-ENTRY CG-SM-OPEN CG-SM-PARAMS 1 MATRIX-REG 2 MATRIX-REG K CG-SM-RET CG-SM-CLOSE"
   EE-WRITE-DRIVER
   EE-ARGV-COMMON
   s" lib/ptx/cg-collective.f" >LEN PROC-ARGV+
   s" lib/ptx/collective.f"    >LEN PROC-ARGV+
   EE-EMIT drop
   SUMNORM-STRUCT? EE-VERDICT ;

: EE-GEMM ( ptr u8 n -- n ) {: a:ptr u:n :}
   s" habu-grade-gemm" MAKI-GRADE:PREPARE
   s" 256 %BLOCK"  a u
   s" MM-AUTHOR-OPEN K MM-AUTHOR-CLOSE"
   EE-WRITE-DRIVER
   EE-ARGV-COMMON
   s" lib/ptx/cg-matmul.f" >LEN PROC-ARGV+
   EE-EMIT drop
   GEMM-STRUCT? EE-VERDICT ;

: EE-ATTN ( ptr u8 n -- n ) {: a:ptr u:n :}
   s" habu-grade-attn" MAKI-GRADE:PREPARE
   s" 128 %BLOCK"  a u
   s" ATTN:AUTHOR-OPEN K ATTN:AUTHOR-CLOSE"
   EE-WRITE-DRIVER
   EE-ARGV-COMMON
   s" lib/ptx/cg-attention.f" >LEN PROC-ARGV+
   EE-EMIT drop
   ATTN-STRUCT? EE-VERDICT ;

: EE-SUMNORM-ACT ( -- )
   EE-CAND$ EE-SUMNORM EE-GRADE ! ;

: EE-GEMM-ACT ( -- )
   EE-CAND$ EE-GEMM EE-GRADE ! ;

: EE-ATTN-ACT ( -- )
   EE-CAND$ EE-ATTN EE-GRADE ! ;

public

: GRADE-SUMNORM ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u CHECK-PASSES? 0= if 0 exit then
   a u EE-CAND!
   [: EE-SUMNORM-ACT ;] catch EE-CLEAN-THROW
   EE-GRADE @ ;

: GRADE-GEMM ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u CHECK-PASSES? 0= if 0 exit then
   a u EE-CAND!
   [: EE-GEMM-ACT ;] catch EE-CLEAN-THROW
   EE-GRADE @ ;

: GRADE-ATTN ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u CHECK-PASSES? 0= if 0 exit then
   a u EE-CAND!
   [: EE-ATTN-ACT ;] catch EE-CLEAN-THROW
   EE-GRADE @ ;

;package
