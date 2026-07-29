\ rocq-run.f - the one way a parity gate calls the proof assistant.
\
\ The module lives in `package ROCQ-CMD`. Both Rocq parity gates - the compiler
\ identity gate and the interning gate - compile files under `formal/` and read
\ what the run printed, and the logical mapping they compile under has to be the
\ same one `formal/_CoqProject` declares. Written twice it could diverge, and a
\ gate compiling under the wrong mapping would be asking about a different
\ module, so it is written here once.
\
\ `/usr/bin/env` is the path lookup: the proof assistant is a toolchain
\ dependency on PATH, not a file at a fixed place in the tree. A gate that calls
\ this fails outright on a host without `rocq`, which is why both gates run in the
\ standalone stdlib gate rather than in the resident fast tier.

require lib/prelude.f
require lib/errors.f
require lib/string.f
require lib/test.f
require lib/test/outcome.f
require lib/process-command.f

package ROCQ-CMD
private

300000 constant ROCQ-MS

: ARGS ( -- )
   PROC-CMD:RESET
   s" rocq" >LEN PROC-CMD:ARG+
   s" compile" >LEN PROC-CMD:ARG+
   s" -q" >LEN PROC-CMD:ARG+
   s" -Q" >LEN PROC-CMD:ARG+
   s" formal" >LEN PROC-CMD:ARG+
   s" Habu" >LEN PROC-CMD:ARG+ ;

public

\ Compile one file and assert it compiled. The caller sets the label, because
\ what a failed compile means differs between "the committed proofs no longer
\ build" and "the generated obligations are not provable".
: COMPILE ( ptr u8 n -- ) {: a:ptr u:n :}
   ARGS
   a u >LEN PROC-CMD:ARG+
   s" /usr/bin/env" >LEN ROCQ-MS >MS PROC-CMD:RUN-OUTCOME 0 T-OUTCOME-EXITED= ;

: OUT$ ( -- ptr u8 n )
   PROC-CMD:OUT$ ;

;package
