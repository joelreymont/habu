\ sentinel-test.f - checked tests for the device-readback poison sentinel.

require lib/test.f
require lib/ptx/sentinel.f

package PTXSENT-TEST

create BUF 16 allot

: FILLS-POISON ( -- )                          \ FILL writes POISON to every 4-byte cell
   BUF 16 PTXSENT:FILL
   BUF          PTXSENT:WORD@ PTXSENT:POISON T=
   BUF 4 +      PTXSENT:WORD@ PTXSENT:POISON T=
   BUF 8 +      PTXSENT:WORD@ PTXSENT:POISON T=
   BUF 12 +     PTXSENT:WORD@ PTXSENT:POISON T= ;

: GUARD-PASSES ( -- )                          \ a real readback value passes through unchanged
   $40C00000 PTXSENT:GUARD $40C00000 T= ;

: GUARD-THROWS ( -- )                          \ a cell still holding the sentinel fails closed
   [: PTXSENT:POISON PTXSENT:GUARD drop ;] E-PTX-READBACK TTHROWSQ ;

public

: RUN ( -- )
   T-RESET
   FILLS-POISON
   GUARD-PASSES
   GUARD-THROWS
   T-REPORT ;

;package

PTXSENT-TEST:RUN
