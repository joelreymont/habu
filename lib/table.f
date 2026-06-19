\ table.f - checked fixed-capacity cell table helpers.

2 constant TBL-PAIR-CELLS

: TBL-CHECK-ROW ( n n -- ) {: rows row :}
   rows 0 < if E-TBL-BOUNDS throw then
   row 0 < if E-TBL-BOUNDS throw then
   row rows >= if E-TBL-BOUNDS throw then ;

: TBL-CHECK-FIELD ( n n -- ) {: fields field :}
   fields 0 <= if E-TBL-FIELD throw then
   field 0 < if E-TBL-FIELD throw then
   field fields >= if E-TBL-FIELD throw then ;

: TBL-CHECK-PAIR ( n n -- ) {: fields field :}
   fields field TBL-CHECK-FIELD
   fields field 1 + TBL-CHECK-FIELD ;

: TBL-CELLS ( n n -- n ) {: rows fields :}
   rows 0 < if E-TBL-BOUNDS throw then
   fields 0 <= if E-TBL-FIELD throw then
   rows fields * ;

: TBL-FIELD ( ptr a n n n n -- ptr a ) {: tbl:ptr rows fields row field :}
   rows row TBL-CHECK-ROW
   fields field TBL-CHECK-FIELD
   tbl row fields * field + cells + ;

: TBL-CELL@ ( ptr a n n n n -- a )
   TBL-FIELD @ ;

: TBL-CELL! ( a ptr a n n n n -- ) {: value tbl:ptr rows fields row field :}
   value tbl rows fields row field TBL-FIELD ! ;

: TBL-N@ ( ptr a n n n n -- n )
   TBL-CELL@ ;

: TBL-N! ( n ptr a n n n n -- )
   TBL-CELL! ;

: TBL-BOOL@ ( ptr a n n n n -- bool )
   TBL-CELL@ ;

: TBL-BOOL! ( bool ptr a n n n n -- )
   TBL-CELL! ;

: TBL-A@ ( ptr a n n n n -- ptr u8 )
   TBL-CELL@ ;

: TBL-A! ( ptr u8 ptr a n n n n -- )
   TBL-CELL! ;

: TBL-PAIR! ( ptr u8 n ptr a n n n n -- ) {: a:ptr u tbl:ptr rows fields row field :}
   u 0 < if E-TBL-BOUNDS throw then
   rows row TBL-CHECK-ROW
   fields field TBL-CHECK-PAIR
   a tbl rows fields row field TBL-A!
   u tbl rows fields row field 1 + TBL-N! ;

: TBL-PAIR$ ( ptr a n n n n -- ptr u8 n ) {: tbl:ptr rows fields row field :}
   rows row TBL-CHECK-ROW
   fields field TBL-CHECK-PAIR
   tbl rows fields row field TBL-A@
   tbl rows fields row field 1 + TBL-N@ ;
