\ assert-test.f - focused tests for src/lib/test/assert.f through src/lib/test.f.
\ Run: bin/hb --load src/lib/test.f src/lib/test/assert-test.f

require src/lib/string.f
require src/lib/test.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/test/runner.f
require src/lib/task.f

TASK:MIN-STACK TASK:TASK TAT-COUNT-A
TASK:MIN-STACK TASK:TASK TAT-COUNT-B
200000 constant TAT-COUNT-EACH
variable TAT-COUNT-READY

\ Failure output has shared formatting state, so workers exercise its counter
\ directly while passing assertions exercise the public assertion path.
: TAT-COUNT-WORK ( -- )
   1 TAT-COUNT-READY atomic-add drop
   begin TAT-COUNT-READY atomic@ 2 < while TASK:PAUSE repeat
   TAT-COUNT-EACH 0 ?do
      true T-ASSERT
      T-FAIL+
   loop
   0 TASK:RETURN ;

: TAT-COUNT-JOIN ( ptr n -- )
   TASK:JOIN MATCH result
      ok OF drop ENDOF
      err OF throw ENDOF
   ;MATCH ;

: TAT-COUNT-CONCURRENT ( -- )
   T-RESET
   0 TAT-COUNT-READY !
   ['] TAT-COUNT-WORK TAT-COUNT-A TASK:ACTIVATE
   ['] TAT-COUNT-WORK TAT-COUNT-B TASK:ACTIVATE
   TAT-COUNT-A TAT-COUNT-JOIN
   TAT-COUNT-B TAT-COUNT-JOIN
   T-FAILURES TAT-COUNT-EACH 2 * <> if
      s" assert-test: concurrent failure count" T-EX-FAIL die then
   T-CASES TAT-COUNT-EACH 2 * <> if
      s" assert-test: concurrent case count" T-EX-FAIL die then
   T-RESET ;

: TT-THROW-5 ( -- )
   5 throw ;

\ A deadline is no verdict: a throw check that wants another code lets
\ E-PROC-TIMEOUT go uncaught and counts nothing, so the gate pool reports the
\ row as a timeout; a check that wants the deadline still catches it.
: TT-THROW-DEADLINE ( -- )
   E-PROC-TIMEOUT throw ;

: TT-DEADLINE-WANT-OTHER ( -- )
   [: TT-THROW-DEADLINE ;] E-PROC-TRUNCATED TTHROWSQ ;

: TT-DEADLINE-PASSES ( -- )
   [: TT-DEADLINE-WANT-OTHER ;] catch E-PROC-TIMEOUT T= ;

\ A throw check run inside the xt of another keeps each expected code with its
\ own call: the inner check wants 5, then the outer xt throws the outer's code.
: TT-INNER-WANTS-5 ( -- )
   [: TT-THROW-5 ;] 5 TTHROWSQ ;

: TT-NESTED-THEN-7 ( -- )
   TT-INNER-WANTS-5 7 throw ;

: TT-NESTED-THEN-5 ( -- )
   TT-INNER-WANTS-5 TT-THROW-5 ;

\ A child engine's deliberately failing T= pins the one-line
\ "assert: expected 3 got 9" shape src/lib/test/assert.f prints.
create TAT-SCRIPT-PATH FS-PATH-CAP allot
variable TAT-SCRIPT-U

: TAT-LF ( -- )
   10 SB-APPEND-C ;

: TAT-SCRIPT$ ( -- ptr u8 n )
   TAT-SCRIPT-PATH TAT-SCRIPT-U @ ;

: TAT-FAIL-SRC$ ( -- ptr u8 n )
   SB-RESET
   s" require src/lib/test.f" SB-APPEND TAT-LF
   s" T-RESET" SB-APPEND TAT-LF
   s" 9 3 T=" SB-APPEND TAT-LF
   s" T-REPORT" SB-APPEND TAT-LF
   SB$ ;

: TAT-PREPARE ( -- )
   s" habu-assert-line" GT-START
   s" fail.f" TAT-SCRIPT-PATH GT-PATH TAT-SCRIPT-U !
   TAT-SCRIPT$ TAT-FAIL-SRC$ WRITE-ALL ;

: TAT-RUN ( -- )
   PROC-ARGV-RESET
   TAT-SCRIPT$ >LEN PROC-ARGV+
   s" bin/hb" GT-DEFAULT-TIMEOUT-MS GT-RUN ;

\ Deliberate failures are reset between cases. Check their count before a
\ later assertion could turn a missing failure into the expected count.
: TAT-EXPECT-FAILURES ( n -- )
   T-FAILURES <> if s" assert-test: unexpected failure count" T-EX-FAIL die then ;

\ A failing numeric assert renders through FMT's private buffer, so a case that
\ is part-way through the shared builder keeps what it built. Seeding SB with
\ text that is not the printed number is what makes the check bite: a printer
\ that reset the builder would leave the failed number's digits behind.
: TAT-SB-SURVIVES-FAILURE ( -- )
   T-RESET
   SB-RESET s" keep-me" SB-APPEND
   1 2 T=
   1 TAT-EXPECT-FAILURES
   SB$ s" keep-me" T$=
   1 TAT-EXPECT-FAILURES
   1 1 T<>
   2 TAT-EXPECT-FAILURES
   SB$ s" keep-me" T$=
   2 TAT-EXPECT-FAILURES ;

: TAT-TEST-ONE-LINE ( -- )
   TAT-PREPARE
   TAT-RUN
   T-EX-FAIL s" assert-line rc" GT-RC=
   s" assert: expected 3 got 9" s" assert-line one line" GT-STDOUT-HAS
   GT-CLEANUP
   GT-FAILURES 0 T= ;

T-RESET
s" numeric mismatch" T-LABEL
1 2 T=
1 TAT-EXPECT-FAILURES
T-CASES 1 T=
1 TAT-EXPECT-FAILURES

T-RESET
T-FAIL+
1 TAT-EXPECT-FAILURES

T-RESET
' TT-THROW-5 4 TTHROWS
1 TAT-EXPECT-FAILURES
T-CASES 1 T=
1 TAT-EXPECT-FAILURES

T-RESET
TT-DEADLINE-PASSES
0 TAT-EXPECT-FAILURES
T-CASES 1 T=
' TT-THROW-DEADLINE E-PROC-TIMEOUT TTHROWS
0 TAT-EXPECT-FAILURES

\ The outer check passes on its own code and fails on the inner's.
T-RESET
' TT-NESTED-THEN-7 7 TTHROWS
0 TAT-EXPECT-FAILURES
T-CASES 2 T=
0 TAT-EXPECT-FAILURES
T-RESET
' TT-NESTED-THEN-5 9 TTHROWS
1 TAT-EXPECT-FAILURES

TAT-SB-SURVIVES-FAILURE
TAT-COUNT-CONCURRENT

T-RESET

T-LABEL$ s" " T$=
s" alpha-label" T-LABEL
T-LABEL$ s" alpha-label" T$=
T-LABEL$ s" " T$=
s" clear-label" T-LABEL
T-LABEL-CLEAR
T-LABEL$ s" " T$=
s" true label" T-LABEL
true TTRUE
T-LABEL$ s" " T$=

TAT-TEST-ONE-LINE

T-REPORT
