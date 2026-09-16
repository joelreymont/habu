\ assert-test.f - focused tests for lib/test/assert.f through lib/test.f.
\ Run: bin/hb --load lib/test.f lib/test/assert-test.f

require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/test/runner.f

: TT-THROW-7 ( -- )
   7 throw ;

: TT-THROW-5 ( -- )
   5 throw ;

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
T-CASES 1 T=
T-FAILURES 1 T=

T-RESET
T-FAIL+
T-FAILURES 1 T=

T-RESET
' TT-THROW-5 4 TTHROWS
T-CASES 1 T=
T-FAILURES 1 T=

T-RESET

1 1 T=
2 3 T<>
-1 TTRUE
0 TFALSE
s" alpha" s" alpha" T$=
s" alpha" s" beta" T$<>
' TT-THROW-7 7 TTHROWS
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

T-CASES 14 T=
T-FAILURES 0 T=
T-REPORT
