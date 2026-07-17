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

: OK ( -- )
   s" subject fork captures normal evaluation" T-LABEL
   s" 73 . cr" TIMEOUT-MS >MS RUN
   0 T-OUTCOME-EXITED=
   LEN>N {: erru:n :}
   LEN>N {: outu:n :}
   OUT outu s" 73" CONTAINS? TTRUE
   erru 0 T= ;

: ISOLATED ( -- )
   s" child dictionary mutation is isolated" T-LABEL
   s" : SUBJECT-CHILD-WORD ( -- n ) 73 ;" TIMEOUT-MS >MS RUN
   0 T-OUTCOME-EXITED=
   LEN>N drop LEN>N drop
   s" SUBJECT-CHILD-WORD" 0 search-wl 0= TTRUE ;

: REJECTED ( -- )
   s" checked rejection preserves rc and diagnostic" T-LABEL
   s" SUBJECT-MISSING" TIMEOUT-MS >MS RUN
   70 T-OUTCOME-EXITED=
   LEN>N {: erru:n :}
   LEN>N {: outu:n :}
   outu 0 T=
   ERR erru s" E-UNDEFINED: SUBJECT-MISSING" CONTAINS? TTRUE ;

: REPL-NOOP ( -- ) ;

: PARENT-PROBE ( -- )
   ['] REPL-NOOP data-base REPLH-CELL + !
   s" SUBJECT-MISSING" TIMEOUT-MS >MS RUN
   70 T-OUTCOME-EXITED=
   LEN>N {: erru:n :}
   LEN>N {: outu:n :}
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
   s" begin again" SHORT-MS >MS RUN
   T-OUTCOME-TIMEOUT
   LEN>N drop LEN>N drop ;

public

: TEST ( -- )
   T-RESET
   OK
   ISOLATED
   REJECTED
   PARENT-HANDLERS
   TIMED
   T-REPORT
   s" subject-test: ok" type cr ;

;package

SUBJECT-TEST:TEST
