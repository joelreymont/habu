\ engine-size-test.f - emitted-engine region measurement regressions.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f

package HB-SIZE-LOAD

: WORD? ( ptr u8 n -- bool )
   XREF-FIND 0= 0= ;

public

: LOAD ( -- )
   s" ASM-INIT" WORD? 0= if
      s" src/arch/arm64/asm.f" included
      s" src/arch/arm64/icode.f" included
   then
   s" HB-SIZE:MARK" WORD? 0= if s" src/habu/engine-size.f" included then ;

;package

HB-SIZE-LOAD:LOAD

package HB-SIZE-TEST

: ROWS ( -- )
   T-RESET
   ASM-INIT
   HB-SIZE:RESET
   s" empty" HB-SIZE:MARK
   3 ASM-CP !
   s" three" HB-SIZE:MARK
   HB-SIZE:COUNT 2 T=
   0 HB-SIZE:NAME$ s" empty" T$=
   1 HB-SIZE:NAME$ s" three" T$=
   0 HB-SIZE:END@ 0 T=
   1 HB-SIZE:END@ 12 T=
   0 HB-SIZE:BYTES@ 0 T=
   1 HB-SIZE:BYTES@ 12 T=
   T-REPORT ;

ROWS

;package
