\ table.f - checked fixed-capacity cell table helpers.

2 constant TBL-PAIR-CELLS
$7FFFFFFFFFFFFFFF constant TBL-MAX-CELLS

: TBL-CHECK-ROW ( count idx -- ) {: rows row :}
   rows COUNT>N 0 < if E-TBL-BOUNDS throw then
   row IDX>N 0 < if E-TBL-BOUNDS throw then
   row IDX>N rows COUNT>N >= if E-TBL-BOUNDS throw then ;

: TBL-CHECK-FIELD ( count idx -- ) {: fields field :}
   fields COUNT>N 0 <= if E-TBL-FIELD throw then
   field IDX>N 0 < if E-TBL-FIELD throw then
   field IDX>N fields COUNT>N >= if E-TBL-FIELD throw then ;

: TBL-CHECK-PAIR ( count idx -- ) {: fields field :}
   fields field TBL-CHECK-FIELD
   fields field IDX>N 1 + >IDX TBL-CHECK-FIELD ;

: TBL-CELLS ( count count -- count ) {: rows fields :}
   rows COUNT>N 0 < if E-TBL-BOUNDS throw then
   fields COUNT>N 0 <= if E-TBL-FIELD throw then
   fields COUNT>N 0 > rows COUNT>N TBL-MAX-CELLS fields COUNT>N / > and if E-TBL-BOUNDS throw then
   rows COUNT>N fields COUNT>N * >COUNT ;

: TBL-FIELD ( ptr a count count idx idx -- ptr a ) {: tbl:ptr rows fields row field :}
   rows row TBL-CHECK-ROW
   fields field TBL-CHECK-FIELD
   tbl row IDX>N fields COUNT>N * field IDX>N + >OFF OFF>N cells + ;

: TBL-CELL@ ( ptr a count count idx idx -- a )
   TBL-FIELD @ ;

: TBL-CELL! ( a ptr a count count idx idx -- ) {: value tbl:ptr rows fields row field :}
   value tbl rows fields row field TBL-FIELD ! ;

: TBL-N@ ( ptr a count count idx idx -- n )
   TBL-CELL@ ;

: TBL-N! ( n ptr a count count idx idx -- )
   TBL-CELL! ;

: TBL-BOOL@ ( ptr a count count idx idx -- bool )
   TBL-CELL@ ;

: TBL-BOOL! ( bool ptr a count count idx idx -- )
   TBL-CELL! ;

: TBL-A@ ( ptr a count count idx idx -- ptr u8 )
   TBL-CELL@ ;

: TBL-A! ( ptr u8 ptr a count count idx idx -- )
   TBL-CELL! ;

: TBL-PAIR! ( ptr u8 len ptr a count count idx idx -- ) {: a:ptr u tbl:ptr rows fields row field :}
   u LEN>N 0 < if E-TBL-BOUNDS throw then
   rows row TBL-CHECK-ROW
   fields field TBL-CHECK-PAIR
   a tbl rows fields row field TBL-A!
   u LEN>N tbl rows fields row field IDX>N 1 + >IDX TBL-N! ;

: TBL-PAIR$ ( ptr a count count idx idx -- ptr u8 len ) {: tbl:ptr rows fields row field :}
   rows row TBL-CHECK-ROW
   fields field TBL-CHECK-PAIR
   tbl rows fields row field TBL-A@
   tbl rows fields row field IDX>N 1 + >IDX TBL-N@ >LEN ;
