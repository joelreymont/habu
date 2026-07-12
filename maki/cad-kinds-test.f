\ cad-kinds-test.f - nominal Model CAD kind checker regressions.

require lib/test.f
require lib/string.f
require test/checker-assert.f
require maki/cad-kinds.f

package CAD-KIND-TEST

create BUF 8192 allot

: YES ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! -1 T= ;

: NO ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! 0 T= ;

: DIAG< ( ptr u8 n -- )
   BUF 8192 DIAG-BUFFER!
   0 0= DIAG-JSON!
   CHECK-CANDIDATE! 0 T= ;

: DIAG? ( ptr u8 n -- )
   DIAG-BUFFER$ 2swap CONTAINS? TTRUE ;

: DIAG-END ( -- )
   0 0= 0= DIAG-JSON!
   DIAG-BUFFER-OFF ;

T-RESET

\ Every public declaration resolves by its qualified nominal identity.
s" CK-DESIGN ( CAD-KIND:design-id -- CAD-KIND:design-id )" YES
s" CK-REV ( CAD-KIND:rev-id -- CAD-KIND:rev-id )" YES
s" CK-OBJ ( CAD-KIND:obj-id -- CAD-KIND:obj-id )" YES
s" CK-NODE ( CAD-KIND:node-id -- CAD-KIND:node-id )" YES
s" CK-ANALYSIS ( CAD-KIND:analysis-id -- CAD-KIND:analysis-id )" YES
s" CK-PLAN ( CAD-KIND:plan-id -- CAD-KIND:plan-id )" YES
s" CK-ARTIFACT ( CAD-KIND:artifact-id -- CAD-KIND:artifact-id )" YES
s" CK-EVIDENCE ( CAD-KIND:evidence-id -- CAD-KIND:evidence-id )" YES
s" CK-TARGET ( CAD-KIND:target-id -- CAD-KIND:target-id )" YES
s" CK-TOOLCHAIN ( CAD-KIND:toolchain-id -- CAD-KIND:toolchain-id )" YES
s" CK-PASS ( CAD-KIND:pass-id -- CAD-KIND:pass-id )" YES
s" CK-SCHEMA ( CAD-KIND:schema-id -- CAD-KIND:schema-id )" YES
s" CK-DIM ( CAD-KIND:dim -- CAD-KIND:dim )" YES
s" CK-SHAPE ( CAD-KIND:shape -- CAD-KIND:shape )" YES
s" CK-ROWS ( CAD-KIND:rows -- CAD-KIND:rows )" YES
s" CK-COLS ( CAD-KIND:cols -- CAD-KIND:cols )" YES
s" CK-DTYPE ( CAD-KIND:dtype -- CAD-KIND:dtype )" YES
s" CK-LAYOUT ( CAD-KIND:layout -- CAD-KIND:layout )" YES
s" CK-SPACE ( CAD-KIND:address-space -- CAD-KIND:address-space )" YES
s" CK-STAGE ( CAD-KIND:stage -- CAD-KIND:stage )" YES
s" CK-EFFECT ( CAD-KIND:effect -- CAD-KIND:effect )" YES
s" CK-REGION ( CAD-KIND:region -- CAD-KIND:region )" YES

\ Adjacent roles never collapse to one cell-shaped scalar identity.
s" CK-X01 ( CAD-KIND:design-id -- CAD-KIND:rev-id )" NO
s" CK-X02 ( CAD-KIND:rev-id -- CAD-KIND:obj-id )" NO
s" CK-X03 ( CAD-KIND:obj-id -- CAD-KIND:node-id )" NO
s" CK-X04 ( CAD-KIND:node-id -- CAD-KIND:analysis-id )" NO
s" CK-X05 ( CAD-KIND:analysis-id -- CAD-KIND:plan-id )" NO
s" CK-X06 ( CAD-KIND:plan-id -- CAD-KIND:artifact-id )" NO
s" CK-X07 ( CAD-KIND:artifact-id -- CAD-KIND:evidence-id )" NO
s" CK-X08 ( CAD-KIND:evidence-id -- CAD-KIND:target-id )" NO
s" CK-X09 ( CAD-KIND:target-id -- CAD-KIND:toolchain-id )" NO
s" CK-X10 ( CAD-KIND:toolchain-id -- CAD-KIND:pass-id )" NO
s" CK-X11 ( CAD-KIND:pass-id -- CAD-KIND:schema-id )" NO
s" CK-X12 ( CAD-KIND:schema-id -- CAD-KIND:dim )" NO
s" CK-X13 ( CAD-KIND:dim -- CAD-KIND:shape )" NO
s" CK-X14 ( CAD-KIND:shape -- CAD-KIND:rows )" NO
s" CK-X15 ( CAD-KIND:rows -- CAD-KIND:cols )" NO
s" CK-X16 ( CAD-KIND:cols -- CAD-KIND:dtype )" NO
s" CK-X17 ( CAD-KIND:dtype -- CAD-KIND:layout )" NO
s" CK-X18 ( CAD-KIND:layout -- CAD-KIND:address-space )" NO
s" CK-X19 ( CAD-KIND:address-space -- CAD-KIND:stage )" NO
s" CK-X20 ( CAD-KIND:stage -- CAD-KIND:effect )" NO
s" CK-X21 ( CAD-KIND:effect -- CAD-KIND:region )" NO

\ Generic typed memory keeps the declared role on store and fetch.
s" CK-PUT ( CAD-KIND:design-id ptr CAD-KIND:design-id -- ) !" YES
s" CK-GET ( ptr CAD-KIND:design-id -- CAD-KIND:design-id ) @" YES
s" CK-BAD-STORE ( CAD-KIND:node-id ptr CAD-KIND:design-id -- ) !" NO

\ Failure packets expose both qualified roles to a repair agent.
s" CK-BAD-ID ( CAD-KIND:design-id -- CAD-KIND:node-id )" DIAG<
s\" \"expected\"" DIAG?
s\" \"actual\"" DIAG?
s" CAD-KIND:design-id" DIAG?
s" CAD-KIND:node-id" DIAG?
DIAG-END

s" CK-BAD-STORE-DIAG ( CAD-KIND:node-id ptr CAD-KIND:design-id -- ) !" DIAG<
s\" \"expected\"" DIAG?
s\" \"actual\"" DIAG?
s" CAD-KIND:design-id" DIAG?
s" CAD-KIND:node-id" DIAG?
DIAG-END

T-REPORT

end-package
