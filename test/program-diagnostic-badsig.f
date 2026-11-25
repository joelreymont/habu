\ program-diagnostic-badsig.f - one rejected signature per definer, each in its
\ own multi-error load, for test/program-diagnostics-test.f BADSIG-ONCE. The
\ check rejects a colon definition's, the stored row a `TRUSTED:` or `defer`
\ one's.
package PROGRAM-DIAG-BADSIG
public
: START ( -- ) CHECK:MULTI-ERR-BEGIN ;
: FINISH ( -- n ) CHECK:MULTI-ERR-END ;
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
s" ok" type cr
