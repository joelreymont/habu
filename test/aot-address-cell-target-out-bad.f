\ A declared address cell the window carries may not hold a target the window
\ does not place: the seed writes the cell's value from its row, so only a
\ window-relative value travels. A raw address of the capturing process would be
\ baked into a product whose window sits elsewhere; leaving the cell out would
\ hand the seeded engine whatever bytes sit at that offset. This fixture is the
\ refusal's one deliberate producer (src/habu/aot-capture.f ACAP-BAKE-DATA).

require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/habu/layout.f
require src/habu/aot-decl.f
require src/habu/aot-arm.f
require src/habu/aot-capture.f

package AOT-XTCELL-TARGET-OUT-PRE
public

\ Created before the window opens: the address belongs to this process and no
\ target carries it.
create OUTSIDE 8 allot

;package

AOT-ARM:WINDOW-OPEN
package AOT-XTCELL-TARGET-OUT
public

\ The declaration makes the capture read the cell. The store after WINDOW-CLOSE
\ leaves the cell's content the only thing under test and the window's extent
\ exactly SLOT's eight bytes.
PERSISTED-PTR-VARIABLE SLOT

;package
AOT-ARM:WINDOW-CLOSE
AOT-XTCELL-TARGET-OUT-PRE:OUTSIDE AOT-XTCELL-TARGET-OUT:SLOT !
AOT-ARM:R0 @ AOT-ARM:D0 @ AOT-CAPTURE:PRELUDE-MARK
s" AOT-XTCELL-TARGET-OUT-ARMED" type cr
AOT-ARM:WINDOW$ AOT-CAPTURE:CAPTURE
s" CAPTURED" type cr
