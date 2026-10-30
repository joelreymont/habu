\ program-diagnostic-badsig.f - one refused signature per definer, each in its
\ own multi-error load, for test/program-diagnostics-test.f BADSIG-ONCE. The
\ check refuses a colon definition's, the stored row a `TRUSTED:` or `defer`
\ one's; the native `;` (T0, tier 1) abandons a refused `TRUSTED:` unpublished.
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
