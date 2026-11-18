split-str
\ A recovered declaration still reports the word that failed checking.
package PROGRAM-DIAG-ME
public
: START ( -- ) CHECK:MULTI-ERR-BEGIN ;
: FINISH ( -- n ) CHECK:MULTI-ERR-END ;
;package

PROGRAM-DIAG-ME:START
s" : MEA1 ( n -- n ) drop ;" evaluate
PROGRAM-DIAG-ME:FINISH drop
s" ok" type cr
