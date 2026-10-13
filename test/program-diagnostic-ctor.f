\ A duplicate family reports its declaration while the load completes.
package PROGRAM-DIAG-CTOR
SUMTYPE zres VARIANT ok n ;VARIANT ;SUMTYPE
public
: START ( -- ) MULTI-ERR-BEGIN ;
: FINISH ( -- n ) MULTI-ERR-END ;
;package

PROGRAM-DIAG-CTOR:START
s" package PROGRAM-DIAG-CTOR SUMTYPE zres VARIANT no n ;VARIANT ;SUMTYPE ;package" evaluate
PROGRAM-DIAG-CTOR:FINISH drop
s" ok" type cr
