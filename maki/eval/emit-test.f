\ maki/eval/emit-test.f - the off-device authoring autograder suite.
\
\ Each task probes the verdict classes of maki/eval/emit.f: a correct kernel is
\ GREEN(2); some type-identical semantic bugs certify but trip a forbidden/
\ missing PTX instruction and grade TYPED-WRONG(1); an ill-typed kernel is
\ REJECTED(0) before any emit.
\
\ HONEST LIMIT (the emit-level structural gates are NOT numeric goldens): a
\ same-type ROLE or VALUE swap keeps every required instruction and grades
\ GREEN(2) — sumnorm in/out swap, div-by-sum-squared, gemm double-accumulate,
\ attention Q/K swap and output-into-V all pass structure while computing the
\ wrong result. The `wrong-but-green` block below pins those at their real
\ grade 2 so a future grader strengthening flips them intentionally, not
\ silently. The attention task is NOT free of
\ reachable non-zero wrong kernels: reorder/omission reject at 0, but a role
\ swap grades 2. What the checker's linear-use discipline DOES catch for free
\ is a dead normalized value (pinned as a 0 below).

require lib/test.f
require maki/eval/emit.f
require maki/eval/emit-device.f   \ shared wrong-but-green fixtures

T-RESET

package EVAL

: TEST-MISSING-DRIVER$ ( -- ptr u8 n )
   s" maki/no-such-eval-driver.f" ;

: TEST-EMPTY-DRIVER$ ( -- ptr u8 n )
   s" /dev/null" ;

: TEST-GEMM ( -- )
   s" K ( matrix<space-global,f32,extent-m,extent-k> matrix<space-global,f32,extent-k,extent-n> matrix<space-global,f32,extent-m,extent-n> -- ) MM-BEGIN MM-K-LOOP MM-STORE"
   GRADE-GEMM drop ;

: TEST-BROKEN-CHILD ( -- )
   [: TEST-MISSING-DRIVER$ ;] is EE-DRIVER$
   [: TEST-GEMM ;] catch {: rc:n :}
   EE-DRIVER-LIVE!
   rc throw ;

: TEST-ZERO-PTX ( -- )
   [: TEST-EMPTY-DRIVER$ ;] is EE-DRIVER$
   [: TEST-GEMM ;] catch {: rc:n :}
   EE-DRIVER-LIVE!
   rc throw ;

: TEST-CLEAN? ( -- bool )
   MAKI-GRADE:DRIVER$ FILE? 0= ;

' TEST-BROKEN-CHILD E-EVAL-EMIT TTHROWS
TEST-CLEAN? TTRUE
EE-ERR$ s" cannot open" CONTAINS? TTRUE
' TEST-ZERO-PTX E-EVAL-EMIT TTHROWS
TEST-CLEAN? TTRUE

;package

\ ---- sumnorm: out[r] = in[r] / sum(in[r]) ------------------------------------
\ shared fixtures (EVND:*) so the structural grade and the device golden
\ (maki/eval/emit-device.f) judge the EXACT same candidate source.
s" sumnorm green" T-LABEL
   EVND:SN-GREEN$ EVAL:GRADE-SUMNORM 2 T=
\ BLOCK-MAX for BLOCK-SUM is TYPE-IDENTICAL: certifies, forbidden max.f32 trips
s" sumnorm max-not-sum" T-LABEL
   s" K ( matrix<space-global,f32,extent-r,extent-c> matrix<space-global,f32,extent-r,extent-c> -- ) {: in out :} ROW {: r :} in r ROW-SPAN {: xs :} xs ROW-CTX {: c :} xs c ROW-LOAD {: x :} x BLOCK-MAX {: s :} x s PTX:B/ out r ROW-SPAN c ROW-STORE"
   EVAL:GRADE-SUMNORM 1 T=
\ softmax pattern-match (EXP. of the shifted row) certifies; forbidden ex2 trips
s" sumnorm softmaxed" T-LABEL
   s" K ( matrix<space-global,f32,extent-r,extent-c> matrix<space-global,f32,extent-r,extent-c> -- ) {: in out :} ROW {: r :} in r ROW-SPAN {: xs :} xs ROW-CTX {: c :} xs c ROW-LOAD {: x :} x BLOCK-MAX {: mx :} x mx PTX:B- EXP. {: e :} e BLOCK-SUM {: s :} e s PTX:B/ out r ROW-SPAN c ROW-STORE"
   EVAL:GRADE-SUMNORM 1 T=
\ missing ROW-STORE -> checker reject
s" sumnorm no-store" T-LABEL
   s" K ( matrix<space-global,f32,extent-r,extent-c> matrix<space-global,f32,extent-r,extent-c> -- ) {: in out :} ROW {: r :} in r ROW-SPAN {: xs :} xs ROW-CTX {: c :} xs c ROW-LOAD {: x :} x BLOCK-SUM {: s :} x s PTX:B/"
   EVAL:GRADE-SUMNORM 0 T=

\ ---- gemm: C = A*B through the checked phase words ---------------------------
s" gemm green" T-LABEL
   EVND:GEMM-GREEN$ EVAL:GRADE-GEMM 2 T=
\ skipping MM-K-LOOP certifies (mmctx/mmracc are phase-neutral through it) but
\ emits no fma/cp.async -> the structural gate catches the zero-compute GEMM
s" gemm no-k-loop" T-LABEL
   s" K ( matrix<space-global,f32,extent-m,extent-k> matrix<space-global,f32,extent-k,extent-n> matrix<space-global,f32,extent-m,extent-n> -- ) MM-BEGIN MM-STORE"
   EVAL:GRADE-GEMM 1 T=
\ missing MM-STORE leaves mmctx+mmacc on the stack -> checker reject
s" gemm no-store" T-LABEL
   s" K ( matrix<space-global,f32,extent-m,extent-k> matrix<space-global,f32,extent-k,extent-n> matrix<space-global,f32,extent-m,extent-n> -- ) MM-BEGIN MM-K-LOOP"
   EVAL:GRADE-GEMM 0 T=

\ ---- attention: O = softmax(Q*K^T)*V through the phase-token pipeline --------
s" attn green" T-LABEL
   EVND:ATTN-GREEN$ EVAL:GRADE-ATTN 2 T=
\ softmax before score: the attnctx stage token rejects the reorder
s" attn reorder" T-LABEL
   s" K ( matrix<space-global,f32,extent-q,extent-d> matrix<space-global,f32,extent-q,extent-d> matrix<space-global,f32,extent-q,extent-d> matrix<space-global,f32,extent-q,extent-d> -- ) ATTN:START ATTN:STAGE-Q ATTN:SOFTMAX ATTN:SCORE ATTN:OUTPUT ATTN:FINISH"
   EVAL:GRADE-ATTN 0 T=
\ omitting the softmax phase: stage-softmax cannot reach OUTPUT
s" attn no-softmax" T-LABEL
   s" K ( matrix<space-global,f32,extent-q,extent-d> matrix<space-global,f32,extent-q,extent-d> matrix<space-global,f32,extent-q,extent-d> matrix<space-global,f32,extent-q,extent-d> -- ) ATTN:START ATTN:STAGE-Q ATTN:SCORE ATTN:OUTPUT ATTN:FINISH"
   EVAL:GRADE-ATTN 0 T=

\ ---- wrong-but-green: same-type semantic bugs the STRUCTURAL gates miss ------
\ These certify and emit the required instructions, exposing the structural
\ grader's semantic limit.
s" wrong-but-green sumnorm in/out swap [structural limit]" T-LABEL
   EVND:SN-SWAP$ EVAL:GRADE-SUMNORM 2 T=
s" wrong-but-green sumnorm div-by-sum-squared [structural limit]" T-LABEL
   EVND:SN-SQSUM$ EVAL:GRADE-SUMNORM 2 T=
s" wrong-but-green gemm double-accumulate [structural limit]" T-LABEL
   EVND:GEMM-DOUBLE$ EVAL:GRADE-GEMM 2 T=
s" wrong-but-green attn Q/K swap [structural limit]" T-LABEL
   EVND:ATTN-QK$ EVAL:GRADE-ATTN 2 T=
s" wrong-but-green attn output-into-V [structural limit]" T-LABEL
   EVND:ATTN-OV$ EVAL:GRADE-ATTN 2 T=
\ the checker's linear-use discipline DOES catch a dead normalized value (0, not 2):
s" reject sumnorm dead-normalize" T-LABEL
   s" K ( matrix<space-global,f32,extent-r,extent-c> matrix<space-global,f32,extent-r,extent-c> -- ) {: in out :} ROW {: r :} in r ROW-SPAN {: xs :} xs ROW-CTX {: c :} xs c ROW-LOAD {: x :} x BLOCK-SUM {: s :} out r ROW-SPAN c x ROW-STORE"
   EVAL:GRADE-SUMNORM 0 T=

T-REPORT
