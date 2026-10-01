\ assert-test.f - focused tests for lib/test/assert.f through lib/test.f.
\ Run: bin/hb --load lib/test.f lib/test/assert-test.f

require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/test/runner.f

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

\ A child engine's deliberately failing T= pins the one-line
\ "assert: expected 3 got 9" shape lib/test/assert.f prints.
create TAT-SCRIPT-PATH FS-PATH-CAP allot
variable TAT-SCRIPT-U

: TAT-LF ( -- )
   10 SB-APPEND-C ;

: TAT-SCRIPT$ ( -- ptr u8 n )
   TAT-SCRIPT-PATH TAT-SCRIPT-U @ ;

: TAT-FAIL-SRC$ ( -- ptr u8 n )
   SB-RESET
   s" require lib/test.f" SB-APPEND TAT-LF
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

TAT-SB-SURVIVES-FAILURE

T-RESET

T-LABEL$ s" " T$=
s" alpha-label" T-LABEL
T-LABEL$ s" alpha-label" T$=
T-LABEL$ s" " T$=
s" clear-label" T-LABEL
T-LABEL-CLEAR
T-LABEL$ s" " T$=
s" true label" T-LABEL
-1 TTRUE
T-LABEL$ s" " T$=

TAT-TEST-ONE-LINE

T-REPORT
