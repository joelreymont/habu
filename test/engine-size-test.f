\ engine-size-test.f - emitted-engine region measurement regressions.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f

\ The three ARM64 encoder sources are `require`d, not probed-and-included.
\ A probe on ASM-INIT decided whether to load asm.f, but ASM-INIT is defined in
\ icode.f, so the probe only ever answered "is icode.f loaded" - it reported
\ asm.f absent whenever the two were not loaded together, and loading asm.f a
\ second time is a duplicate definition, not a no-op. `require` asks the
\ registry that actually records what is loaded, which is also the registry the
\ ten-plus existing `require src/arch/arm64/asm.f` sites already share.
require src/arch/arm64/asm.f
require src/arch/arm64/icode.f

package ENGINE-SIZE-LOAD

: WORD? ( ptr u8 n -- bool )
   XREF-FIND 0= 0= ;

public

: LOAD ( -- )
   s" ENGINE-SIZE:MARK" WORD? 0= if s" src/habu/engine-size.f" included then ;

;package

ENGINE-SIZE-LOAD:LOAD

package ENGINE-SIZE-TEST

: ROWS ( -- )
   T-RESET
   ASM-INIT
   ENGINE-SIZE:RESET
   s" empty" ENGINE-SIZE:MARK
   3 ASM-CP !
   s" three" ENGINE-SIZE:MARK
   ENGINE-SIZE:COUNT 2 T=
   0 ENGINE-SIZE:NAME$ s" empty" T$=
   1 ENGINE-SIZE:NAME$ s" three" T$=
   0 ENGINE-SIZE:END@ 0 T=
   1 ENGINE-SIZE:END@ 12 T=
   0 ENGINE-SIZE:BYTES@ 0 T=
   1 ENGINE-SIZE:BYTES@ 12 T=
   T-REPORT ;

ROWS

;package
