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

s" CK-NODE ( CAD-KIND:node-id -- CAD-KIND:node-id )" YES
s" CK-TARGET ( CAD-KIND:target-id -- CAD-KIND:target-id )" YES
s" CK-NPOLICY ( CAD-KIND:numeric-policy-id -- CAD-KIND:numeric-policy-id )" YES
s" CK-DIM ( CAD-KIND:dim -- CAD-KIND:dim )" YES
s" CK-ROWS ( CAD-KIND:rows -- CAD-KIND:rows )" YES
s" CK-COLS ( CAD-KIND:cols -- CAD-KIND:cols )" YES
s" CK-SPACE ( CAD-KIND:address-space -- CAD-KIND:address-space )" YES
s" CK-REGION ( CAD-KIND:region -- CAD-KIND:region )" YES

s" CK-X01 ( CAD-KIND:node-id -- CAD-KIND:target-id )" NO
s" CK-X02 ( CAD-KIND:target-id -- CAD-KIND:numeric-policy-id )" NO
s" CK-X03 ( CAD-KIND:numeric-policy-id -- CAD-KIND:dim )" NO
s" CK-X04 ( CAD-KIND:dim -- CAD-KIND:rows )" NO
s" CK-X05 ( CAD-KIND:rows -- CAD-KIND:cols )" NO
s" CK-X06 ( CAD-KIND:cols -- CAD-KIND:address-space )" NO
s" CK-X07 ( CAD-KIND:address-space -- CAD-KIND:region )" NO

s" CK-BAD-ID ( CAD-KIND:target-id -- CAD-KIND:node-id )" DIAG<
s\" \"expected\"" DIAG?
s\" \"actual\"" DIAG?
s" cad-kind:node-id<> " DIAG?
s" cad-kind:target-id<> " DIAG?
DIAG-END

1 LAYOUT-BUFFER CKT-RID CAD-KIND:region

s" CKT-PUT ( CAD-KIND:region -- ) 0 CKT-RID !" YES
s" CKT-GET ( -- CAD-KIND:region ) 0 CKT-RID @" YES
s" CKT-ST2 ( n -- CAD-KIND:region ) 0 CKT-RID ! 0 CKT-RID @" NO
variable CKT-CELL
s" CKT-P1 ( -- ptr CAD-KIND:region ) CKT-CELL" NO
s" CKT-P2 ( -- ptr CAD-KIND:cols ) CKT-CELL" NO

T-REPORT

;package
