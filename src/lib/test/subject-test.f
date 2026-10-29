\ subject-test.f - forked subject evaluation capture regressions.

require src/lib/errors.f
require src/lib/string.f
require src/lib/process-command.f
require src/lib/test.f
require src/lib/test/outcome.f
require src/lib/test/subject.f

package SUBJECT-TEST

$800 constant CAP
1000 constant TIMEOUT-MS
1 constant SHORT-MS

create OUT CAP allot
create ERR CAP allot

variable SAVED-REPLH

: RUN ( ptr u8 n ms -- len len outcome ) {: src:ptr srcu:n timeout:ms :}
   src srcu OUT CAP >LEN ERR CAP >LEN timeout SUBJECT:RUN ;

\ Runs the program under TIMEOUT-MS and asserts its exit code, leaving the
\ stdout and stderr lengths.
: EXITS ( ptr u8 n n -- n n ) {: src:ptr srcu:n want:n :}
   src srcu TIMEOUT-MS >MS RUN {: outu:len erru:len oc :}
   src srcu OUT outu LEN>N ERR erru LEN>N oc want T-OUTCOME-EXITED=
   outu LEN>N erru LEN>N ;

: OK ( -- )
   s" subject fork captures normal evaluation" T-LABEL
   s" 73 . cr" 0 EXITS {: outu:n erru:n :}
   OUT outu s" 73" CONTAINS? TTRUE
   erru 0 T= ;

: ISOLATED ( -- )
   s" child dictionary mutation is isolated" T-LABEL
   s" : SUBJECT-CHILD-WORD ( -- n ) 73 ;" 0 EXITS 2drop
   s" SUBJECT-CHILD-WORD" 0 search-wl 0= TTRUE ;

: REJECTED ( -- )
   s" checked rejection preserves rc and diagnostic" T-LABEL
   s" SUBJECT-MISSING" 70 EXITS {: outu:n erru:n :}
   outu 0 T=
   ERR erru s" E-UNDEFINED: SUBJECT-MISSING" CONTAINS? TTRUE ;

: REPL-NOOP ( -- ) ;

: PARENT-PROBE ( -- )
   ['] REPL-NOOP data-base REPLH-CELL + xt!   \ xt! is the declaration point for a code cell
   s" SUBJECT-MISSING" 70 EXITS {: outu:n erru:n :}
   outu 0 T=
   ERR erru s" E-UNDEFINED: SUBJECT-MISSING" CONTAINS? TTRUE ;

: PARENT-HANDLERS ( -- )
   s" child clears inherited parent handlers" T-LABEL
   data-base REPLH-CELL + @ SAVED-REPLH !
   [: PARENT-PROBE ;] catch {: rc:n :}
   SAVED-REPLH @ data-base REPLH-CELL + !
   rc 0 <> if rc throw then ;

: TIMED ( -- )
   s" subject timeout stays an outcome variant" T-LABEL
   \ Bare BEGIN rejects in interpret mode and races the deadline instead of looping.
   s" : SUBJECT-TIMEOUT-HANG ( -- ) begin again ; SUBJECT-TIMEOUT-HANG" SHORT-MS >MS RUN
   T-OUTCOME-TIMEOUT
   LEN>N drop LEN>N drop ;

\ An exit or signal assert handed a deadline reports the case, the program and
\ what the child printed, then throws E-PROC-TIMEOUT. Each probe runs in a child
\ so the parent reads that report back from the child's stdout.
: EXITED-PROBE ( -- )
   s" timed-out probe" T-LABEL
   s" probe program" s" probe stdout" s" probe stderr" OUTCOME:TIMEOUT
   0 T-OUTCOME-EXITED= ;

: SIGNALED-PROBE ( -- )
   s" timed-out probe" T-LABEL
   s" probe program" s" probe stdout" s" probe stderr" OUTCOME:TIMEOUT
   9 T-OUTCOME-SIGNALED= ;

: UNSIGNALED-PROBE ( -- )
   s" unsignaled probe" T-LABEL
   s" probe program" s" probe stdout" s" probe stderr" 3 OUTCOME:EXITED
   9 T-OUTCOME-SIGNALED= ;

: WRONG-SIGNAL-PROBE ( -- )
   s" wrong-signal probe" T-LABEL
   s" probe program" s" probe stdout" s" probe stderr" 9 OUTCOME:SIGNALED
   15 T-OUTCOME-SIGNALED= ;

: EXIT-MISS-PROBE ( -- )
   s" exit-miss probe" T-LABEL
   s" probe program" s" probe stdout" s" probe stderr" 70 OUTCOME:EXITED
   0 T-OUTCOME-EXITED= ;

: SHOWS ( n ptr u8 n -- ) {: outu:n want:ptr wantu:n :}
   OUT outu want wantu CONTAINS? TTRUE ;

: TIMEOUT-REPORT ( ptr u8 n -- ) {: src:ptr srcu:n :}
   src srcu UNCAUGHT-RC EXITS {: outu:n erru:n :}
   outu s" case: timed-out probe" SHOWS
   outu s" probe program" SHOWS
   outu s" probe stdout" SHOWS
   outu s" probe stderr" SHOWS
   \ -2502 is E-PROC-TIMEOUT: the row still reports a timeout.
   ERR erru s" uncaught throw code -2502" CONTAINS? TTRUE ;

: TIMEOUT-REPORTS ( -- )
   s" a timed-out exit assert reports its case and output" T-LABEL
   s" package SUBJECT-TEST EXITED-PROBE ;package" TIMEOUT-REPORT
   s" a timed-out signal assert reports its case and output" T-LABEL
   s" package SUBJECT-TEST SIGNALED-PROBE ;package" TIMEOUT-REPORT ;

\ The probes' program and each stream under its own heading, as SUBJECT:SHOW
\ prints them.
: PROBE-SHOWN ( n -- ) {: outu:n :}
   outu s\" program:\nprobe program\n" SHOWS
   outu s\" stdout:\nprobe stdout\n" SHOWS
   outu s\" stderr:\nprobe stderr\n" SHOWS ;

\ A failed assertion does not throw, so the child exits 0.
: EXIT-MISS-REPORTS ( -- )
   s" an exit assert that misses reports its case and output" T-LABEL
   s" package SUBJECT-TEST EXIT-MISS-PROBE ;package" 0 EXITS drop {: outu:n :}
   outu s" case: exit-miss probe" SHOWS
   outu s" assert: expected 0 got 70" SHOWS
   outu PROBE-SHOWN ;

: UNSIGNALED-REPORTS ( -- )
   s" a signal assert on an exit reports its case and output" T-LABEL
   s" package SUBJECT-TEST UNSIGNALED-PROBE ;package" 0 EXITS drop {: outu:n :}
   outu s" case: unsignaled probe" SHOWS
   outu s" assert: expected signal got exit 3" SHOWS
   outu PROBE-SHOWN ;

: WRONG-SIGNAL-REPORTS ( -- )
   s" a signal assert on another signal reports its case and output" T-LABEL
   s" package SUBJECT-TEST WRONG-SIGNAL-PROBE ;package" 0 EXITS drop {: outu:n :}
   outu s" case: wrong-signal probe" SHOWS
   outu s" assert: expected 15 got 9" SHOWS
   outu PROBE-SHOWN ;

\ T-RUN-EXITED= over /bin/sh -c with this script, for at most ms, wanting this
\ status.
: SH-RUN ( CMD:command ptr u8 n ms n -- CMD:command )
   {: script:ptr su:n deadline:ms status:n :}
   s" -c" CMD:ARG+ script su CMD:ARG+
   s" /bin/sh" deadline status T-RUN-EXITED= ;

: WRITES-EXIT3$ ( -- ptr u8 n ) s" printf run-out; printf run-err >&2; exit 3" ;

: RUN-EXITS ( -- )
   s" a command that exits with the wanted status passes" T-LABEL
   [: WRITES-EXIT3$ TIMEOUT-MS >MS 3 SH-RUN ;] CMD:WITH-COMMAND ;

: RUN-MISS-PROBE ( -- )
   s" run-miss probe" T-LABEL
   [: WRITES-EXIT3$ TIMEOUT-MS >MS 0 SH-RUN ;] CMD:WITH-COMMAND ;

: RUN-SIGNAL-PROBE ( -- )
   s" run-signal probe" T-LABEL
   [: s" printf run-out; printf run-err >&2; kill -9 $$" TIMEOUT-MS >MS 0 SH-RUN ;]
   CMD:WITH-COMMAND ;

: RUN-TIMEOUT-PROBE ( -- )
   s" run timed-out probe" T-LABEL
   [: s" sleep 5" SHORT-MS >MS 0 SH-RUN ;] CMD:WITH-COMMAND ;

\ The program and each stream under its own heading, as SUBJECT:SHOW prints them.
: RUN-SHOWN ( n -- ) {: outu:n :}
   outu s\" program:\n/bin/sh\n" SHOWS
   outu s\" stdout:\nrun-out\n" SHOWS
   outu s\" stderr:\nrun-err\n" SHOWS ;

\ A failed assertion does not throw, so the child exits 0.
: RUN-MISS-REPORTS ( -- )
   s" a command that exits with another status reports its run" T-LABEL
   s" package SUBJECT-TEST RUN-MISS-PROBE ;package" 0 EXITS drop {: outu:n :}
   outu s" case: run-miss probe" SHOWS
   outu s" assert: expected 0 got 3" SHOWS
   outu RUN-SHOWN ;

: RUN-SIGNAL-REPORTS ( -- )
   s" a command killed by a signal reports its run" T-LABEL
   s" package SUBJECT-TEST RUN-SIGNAL-PROBE ;package" 0 EXITS drop {: outu:n :}
   outu s" case: run-signal probe" SHOWS
   outu s" assert: expected exit got signal 9" SHOWS
   outu RUN-SHOWN ;

: RUN-TIMEOUT-REPORTS ( -- )
   s" a command past its deadline throws E-PROC-TIMEOUT" T-LABEL
   s" package SUBJECT-TEST RUN-TIMEOUT-PROBE ;package" UNCAUGHT-RC EXITS {: outu:n erru:n :}
   outu s" case: run timed-out probe" SHOWS
   outu s" /bin/sh" SHOWS
   ERR erru s" uncaught throw code -2502" CONTAINS? TTRUE ;

public

: TEST ( -- )
   T-RESET
   OK
   ISOLATED
   REJECTED
   PARENT-HANDLERS
   TIMED
   TIMEOUT-REPORTS
   EXIT-MISS-REPORTS
   UNSIGNALED-REPORTS
   WRONG-SIGNAL-REPORTS
   RUN-EXITS
   RUN-MISS-REPORTS
   RUN-SIGNAL-REPORTS
   RUN-TIMEOUT-REPORTS
   T-REPORT
   s" subject-test: ok" type cr ;

;package

SUBJECT-TEST:TEST
