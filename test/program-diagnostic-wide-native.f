\ A replacement whose declared input row exceeds stored-signature width.
require src/lib/string.f

package PROGRAM-DIAG-WIDE
public
: START ( -- ) MULTI-ERR-BEGIN ;
: FINISH ( -- n ) MULTI-ERR-END ;
: BAD ( -- )
   SB-RESET
   s" TRUSTED: PDB-WIDE ( " SB-APPEND
   256 0 do s" n " SB-APPEND loop
   s" -- ) ;" SB-APPEND
   SB$ evaluate-closed ;
;package

: PDB-WIDE ( -- ) ;
undefine PDB-WIDE
PROGRAM-DIAG-WIDE:START
' PROGRAM-DIAG-WIDE:BAD catch .
PROGRAM-DIAG-WIDE:FINISH .
s" ok" type cr
