\ A tier-1 replacement whose declared input row exceeds stored-signature width.
require lib/string.f

package PROGRAM-DIAG-WIDE
public
: START ( -- ) MULTI-ERR-BEGIN ;
: FINISH ( -- n ) MULTI-ERR-END ;
TRUSTED: BAD ( -- )
   SB-RESET
   s" TRUSTED: PDB-WIDE ( " SB-APPEND
   256 0 do s" n " SB-APPEND loop
   s" -- ) ;" SB-APPEND
   SB$ evaluate ;
;package

: PDB-WIDE ( -- ) ;
undefine PDB-WIDE
1 set-tier
PROGRAM-DIAG-WIDE:START
' PROGRAM-DIAG-WIDE:BAD catch .
PROGRAM-DIAG-WIDE:FINISH .
s" ok" type cr
