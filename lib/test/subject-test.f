\ subject-test.f - forked subject evaluation capture regressions.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/test/outcome.f
require lib/test/subject.f

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

public

: TEST ( -- )
   T-RESET
   OK
   ISOLATED
   REJECTED
   PARENT-HANDLERS
   TIMED
   TIMEOUT-REPORTS
   T-REPORT
   s" subject-test: ok" type cr ;

;package

SUBJECT-TEST:TEST
