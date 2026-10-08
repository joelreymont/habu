\ Resets the checker by the production path: src/core/checker.f stores its
\ reset xt at RESET-OFF, which tools/native-build-core.f RESET-CHECKER calls.

require src/core/checker-owner-abi.f
require src/habu/layout.f

package OWNER-RESET

: OWNER ( -- ptr u8 ) data-base NCOMP-DISPATCH:DECL-CELL + 0 ptr-field @ ;
CAST: RESET-XT ( n -- [ -- ] )

public

: SOURCE ( -- )
   OWNER CHECKER-OWNER-ABI:RESET-OFF + CELL-VIEW @ RESET-XT execute ;

;package
