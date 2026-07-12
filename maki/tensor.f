\ maki/tensor.f - nominal tensor shape, dtype, layout, and address-space facts.

require maki/cad-kinds.f

-5000 constant E-MK-DTYPE
-5006 constant E-MK-DIM
-5007 constant E-MK-LAYOUT
-5008 constant E-MK-SPACE

package MAKI

\ Zero-arity families are runtime cells. These private identity boundaries are
\ the only raw representation authority; public constructors validate first.
TRUSTED: DIM-REFINE ( n -- CAD-KIND:dim ) ;
TRUSTED: DIM-RAW ( CAD-KIND:dim -- n ) ;
TRUSTED: ROWS-REFINE ( n -- CAD-KIND:rows ) ;
TRUSTED: ROWS-RAW ( CAD-KIND:rows -- n ) ;
TRUSTED: COLS-REFINE ( n -- CAD-KIND:cols ) ;
TRUSTED: COLS-RAW ( CAD-KIND:cols -- n ) ;
TRUSTED: DTYPE-REFINE ( n -- CAD-KIND:dtype ) ;
TRUSTED: DTYPE-RAW ( CAD-KIND:dtype -- n ) ;
TRUSTED: LAYOUT-REFINE ( n -- CAD-KIND:layout ) ;
TRUSTED: LAYOUT-RAW ( CAD-KIND:layout -- n ) ;
TRUSTED: SPACE-REFINE ( n -- CAD-KIND:address-space ) ;
TRUSTED: SPACE-RAW ( CAD-KIND:address-space -- n ) ;

$7FFFFFFFFFFFFFFF constant DIM-MAX-N
5 constant DTYPE-N
2 constant LAYOUT-N
4 constant SPACE-N

: NONNEG ( n -- n )
   dup 0 < if E-MK-DIM throw then ;

: RANGE ( n n n -- n )
   {: value:n limit:n code:n :}
   value 0 < value limit >= or if code throw then
   value ;

: PRODUCT ( n n -- n )
   {: a:n b:n :}
   a NONNEG drop
   b NONNEG drop
   a 0= if 0 exit then
   b DIM-MAX-N a / > if E-MK-DIM throw then
   a b * ;

: SUM-DIM ( n n -- n )
   {: a:n b:n :}
   a NONNEG drop
   b NONNEG drop
   a DIM-MAX-N b - > if E-MK-DIM throw then
   a b + ;

: SUB-DIM ( n n -- n )
   {: a:n b:n :}
   a NONNEG drop
   b NONNEG drop
   a b < if E-MK-DIM throw then
   a b - ;

: SAME-DIM? ( n n -- bool )
   = ;

: BCAST-DIM? ( n n -- bool )
   {: a:n b:n :}
   a b = a 1 = or b 1 = or ;

: MAX-DIM ( n n -- n )
   {: a:n b:n :}
   a b > if a else b then ;

public

: SHAPE ( n n -- CAD-KIND:rows CAD-KIND:cols )
   {: rows:n cols:n :}
   rows NONNEG ROWS-REFINE
   cols NONNEG COLS-REFINE ;

: UNIT-ROWS ( -- CAD-KIND:rows ) 1 ROWS-REFINE ;
: UNIT-COLS ( -- CAD-KIND:cols ) 1 COLS-REFINE ;

: DTYPE-DECODE ( n -- CAD-KIND:dtype )
   DTYPE-N E-MK-DTYPE RANGE DTYPE-REFINE ;

: LAYOUT-DECODE ( n -- CAD-KIND:layout )
   LAYOUT-N E-MK-LAYOUT RANGE LAYOUT-REFINE ;

: ADDRESS-SPACE-DECODE ( n -- CAD-KIND:address-space )
   SPACE-N E-MK-SPACE RANGE SPACE-REFINE ;

: DT-F32 ( -- CAD-KIND:dtype ) 0 DTYPE-REFINE ;
: DT-F16 ( -- CAD-KIND:dtype ) 1 DTYPE-REFINE ;
: DT-BF16 ( -- CAD-KIND:dtype ) 2 DTYPE-REFINE ;
: DT-U32 ( -- CAD-KIND:dtype ) 3 DTYPE-REFINE ;
: DT-I32 ( -- CAD-KIND:dtype ) 4 DTYPE-REFINE ;

: LAY-ROW ( -- CAD-KIND:layout ) 0 LAYOUT-REFINE ;
: LAY-COL ( -- CAD-KIND:layout ) 1 LAYOUT-REFINE ;

: SPACE-HOST ( -- CAD-KIND:address-space ) 0 SPACE-REFINE ;
: SPACE-GLOBAL ( -- CAD-KIND:address-space ) 1 SPACE-REFINE ;
: SPACE-SHARED ( -- CAD-KIND:address-space ) 2 SPACE-REFINE ;
: SPACE-LOCAL ( -- CAD-KIND:address-space ) 3 SPACE-REFINE ;

: DT-VALID? ( n -- bool )
   {: value:n :}
   value 0 < 0= value DTYPE-N < and ;

: LAYOUT-VALID? ( n -- bool )
   {: value:n :}
   value 0 < 0= value LAYOUT-N < and ;

: ADDRESS-SPACE-VALID? ( n -- bool )
   {: value:n :}
   value 0 < 0= value SPACE-N < and ;

: DT-SIZE ( CAD-KIND:dtype -- CAD-KIND:dim )
   DTYPE-RAW case
      0 of 4 DIM-REFINE endof
      1 of 2 DIM-REFINE endof
      2 of 2 DIM-REFINE endof
      3 of 4 DIM-REFINE endof
      4 of 4 DIM-REFINE endof
      E-MK-DTYPE throw
   endcase ;

: DIM* ( CAD-KIND:dim CAD-KIND:dim -- CAD-KIND:dim )
   DIM-RAW swap DIM-RAW swap PRODUCT DIM-REFINE ;

: ROWS+ ( CAD-KIND:rows CAD-KIND:rows -- CAD-KIND:rows )
   ROWS-RAW swap ROWS-RAW swap SUM-DIM ROWS-REFINE ;

: COLS+ ( CAD-KIND:cols CAD-KIND:cols -- CAD-KIND:cols )
   COLS-RAW swap COLS-RAW swap SUM-DIM COLS-REFINE ;

: ROWS- ( CAD-KIND:rows CAD-KIND:rows -- CAD-KIND:rows )
   ROWS-RAW swap ROWS-RAW swap SUB-DIM ROWS-REFINE ;

: COLS- ( CAD-KIND:cols CAD-KIND:cols -- CAD-KIND:cols )
   COLS-RAW swap COLS-RAW swap SUB-DIM COLS-REFINE ;

: DIM-BCAST? ( CAD-KIND:dim CAD-KIND:dim -- bool )
   DIM-RAW swap DIM-RAW swap BCAST-DIM? ;

: DIM-EQUAL? ( CAD-KIND:dim CAD-KIND:dim -- bool )
   DIM-RAW swap DIM-RAW swap SAME-DIM? ;

: DIM-IS? ( CAD-KIND:dim n -- bool )
   {: value:CAD-KIND:dim expected:n :}
   value DIM-RAW expected SAME-DIM? ;

: ROWS-EQUAL? ( CAD-KIND:rows CAD-KIND:rows -- bool )
   ROWS-RAW swap ROWS-RAW swap SAME-DIM? ;

: COLS-EQUAL? ( CAD-KIND:cols CAD-KIND:cols -- bool )
   COLS-RAW swap COLS-RAW swap SAME-DIM? ;

: ROWS-IS? ( CAD-KIND:rows n -- bool )
   {: value:CAD-KIND:rows expected:n :}
   value ROWS-RAW expected SAME-DIM? ;

: COLS-IS? ( CAD-KIND:cols n -- bool )
   {: value:CAD-KIND:cols expected:n :}
   value COLS-RAW expected SAME-DIM? ;

: DTYPE-EQUAL? ( CAD-KIND:dtype CAD-KIND:dtype -- bool )
   DTYPE-RAW swap DTYPE-RAW swap SAME-DIM? ;

: LAYOUT-EQUAL? ( CAD-KIND:layout CAD-KIND:layout -- bool )
   LAYOUT-RAW swap LAYOUT-RAW swap SAME-DIM? ;

: ADDRESS-SPACE-EQUAL? ( CAD-KIND:address-space CAD-KIND:address-space -- bool )
   SPACE-RAW swap SPACE-RAW swap SAME-DIM? ;

: DIM-MAX ( CAD-KIND:dim CAD-KIND:dim -- CAD-KIND:dim )
   DIM-RAW swap DIM-RAW swap MAX-DIM DIM-REFINE ;

: SHAPE-ELEMS ( CAD-KIND:rows CAD-KIND:cols -- CAD-KIND:dim )
   COLS-RAW swap ROWS-RAW swap PRODUCT DIM-REFINE ;

: SHAPE-BCAST? ( CAD-KIND:rows CAD-KIND:cols CAD-KIND:rows CAD-KIND:cols -- bool )
   {: r1:CAD-KIND:rows c1:CAD-KIND:cols r2:CAD-KIND:rows c2:CAD-KIND:cols :}
   r1 ROWS-RAW r2 ROWS-RAW BCAST-DIM?
   c1 COLS-RAW c2 COLS-RAW BCAST-DIM? and ;

: TENSOR-BYTES ( CAD-KIND:rows CAD-KIND:cols CAD-KIND:dtype -- CAD-KIND:dim )
   {: rows:CAD-KIND:rows cols:CAD-KIND:cols dt:CAD-KIND:dtype :}
   rows cols SHAPE-ELEMS dt DT-SIZE DIM* ;

: SHAPE-EQUAL? ( CAD-KIND:rows CAD-KIND:cols CAD-KIND:rows CAD-KIND:cols -- bool )
   {: r1:CAD-KIND:rows c1:CAD-KIND:cols r2:CAD-KIND:rows c2:CAD-KIND:cols :}
   r1 ROWS-RAW r2 ROWS-RAW SAME-DIM?
   c1 COLS-RAW c2 COLS-RAW SAME-DIM? and ;

: SHAPE-IS? ( CAD-KIND:rows CAD-KIND:cols n n -- bool )
   {: rows:CAD-KIND:rows cols:CAD-KIND:cols expected-rows:n expected-cols:n :}
   rows ROWS-RAW expected-rows SAME-DIM?
   cols COLS-RAW expected-cols SAME-DIM? and ;

: BCAST-SHAPE ( CAD-KIND:rows CAD-KIND:cols CAD-KIND:rows CAD-KIND:cols -- CAD-KIND:rows CAD-KIND:cols )
   {: r1:CAD-KIND:rows c1:CAD-KIND:cols r2:CAD-KIND:rows c2:CAD-KIND:cols :}
   r1 c1 r2 c2 SHAPE-BCAST? 0= if E-MK-DIM throw then
   r1 ROWS-RAW r2 ROWS-RAW MAX-DIM ROWS-REFINE
   c1 COLS-RAW c2 COLS-RAW MAX-DIM COLS-REFINE ;

: INNER-EQUAL? ( CAD-KIND:cols CAD-KIND:rows -- bool )
   ROWS-RAW swap COLS-RAW swap SAME-DIM? ;

: TRANSPOSE-SHAPE ( CAD-KIND:rows CAD-KIND:cols -- CAD-KIND:rows CAD-KIND:cols )
   {: rows:CAD-KIND:rows cols:CAD-KIND:cols :}
   cols COLS-RAW ROWS-REFINE
   rows ROWS-RAW COLS-REFINE ;

;package
