\ A recovered declaration still reports the word that failed checking.
package PROGRAM-DIAG-ME
public
: START ( -- ) MULTI-ERR-BEGIN ;
: FINISH ( -- n ) MULTI-ERR-END ;
;package

PROGRAM-DIAG-ME:START
s" : MEA1 ( n -- n ) drop ;" evaluate
PROGRAM-DIAG-ME:FINISH drop
s" ok" type cr
