\ A refused signature is reported and counted once in a multi-error load: the
\ check's own refusal for a colon definition, the stored row's for a
\ `TRUSTED:` definition and a `defer`.
package PROGRAM-DIAG-BADSIG
public
: START ( -- ) MULTI-ERR-BEGIN ;
: FINISH ( -- n ) MULTI-ERR-END ;
: NATIVE-BAD ( -- )
   s" TRUSTED: PDB-NATIVE ( -- zz ) ;" evaluate-closed ;
;package

PROGRAM-DIAG-BADSIG:START
s" : PDB-COLON ( -- zz ) ;" evaluate
PROGRAM-DIAG-BADSIG:FINISH .
PROGRAM-DIAG-BADSIG:START
s" TRUSTED: PDB-TRUSTED ( -- zz ) ;" evaluate
PROGRAM-DIAG-BADSIG:FINISH .
PROGRAM-DIAG-BADSIG:START
s" defer PDB-DEFER ( -- zz )" evaluate
PROGRAM-DIAG-BADSIG:FINISH .
: PDB-NATIVE ( -- ) ;
undefine PDB-NATIVE
1 set-tier
PROGRAM-DIAG-BADSIG:START
' PROGRAM-DIAG-BADSIG:NATIVE-BAD catch .
PROGRAM-DIAG-BADSIG:FINISH .
s" ok" type cr
