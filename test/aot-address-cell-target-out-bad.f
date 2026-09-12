\ A declared address cell the window CARRIES may not hold a target the window does
\ not place.
\
\ WHY THE REFUSAL EXISTS. The cell's bytes travel with the window and the seed
\ writes its value from the row, so the only value a row can carry is a
\ window-relative one. A raw address of the capturing process would be baked into a
\ product whose window sits somewhere else entirely, and leaving the cell out would
\ hand the seeded engine whichever bytes happened to be at that offset.
\
\ WHY IT NEEDS A FIXTURE OF ITS OWN. Until this case was written the refusal had no
\ deliberate producer: it fired on the BOOTING ENGINE's own HOOK-CELL, which is not
\ the window's cell at all and is now left to the engine that owns it (dot
\ habu-keep-declared-addr-dbd7d8d9, src/habu/aot-capture.f ACAP-BAKE-DATA). So the
\ stop that guards a real producer had no test of its own, and every capture taken
\ inside a booted engine died on a row that engine declared at its own boot.
\
\ HOW THE TARGET GETS OUT OF THE WINDOW. OUTSIDE is a field created before the
\ window opens, so its address belongs to this process and no target carries it.
\ SLOT is a PERSISTED-PTR-VARIABLE inside the window - the declaration is what makes
\ the capture read the cell at all - and the store happens after the window closes,
\ so the cell's CONTENT is the only thing under test and the window's own extent is
\ exactly SLOT's eight bytes.

require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/habu/layout.f
require src/habu/aot-decl.f
require src/habu/aot-arm.f
require src/habu/aot-capture.f

package AOT-XTCELL-TARGET-OUT-PRE
public

create OUTSIDE 8 allot

;package

AOT-ARM:WINDOW-OPEN
package AOT-XTCELL-TARGET-OUT
public

PERSISTED-PTR-VARIABLE SLOT

;package
AOT-ARM:WINDOW-CLOSE
AOT-XTCELL-TARGET-OUT-PRE:OUTSIDE AOT-XTCELL-TARGET-OUT:SLOT !
AOT-ARM:R0 @ AOT-ARM:D0 @ AOT-CAPTURE:PRELUDE-MARK
s" AOT-XTCELL-TARGET-OUT-ARMED" type cr
AOT-ARM:WINDOW$ AOT-CAPTURE:CAPTURE
s" CAPTURED" type cr
