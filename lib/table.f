\ table.f - checked fixed-capacity cell table helpers.
\
\ The module lives in `package TBL`. External callers use the qualified public API
\ (TBL:CELL-COUNT, TBL:FIELD, TBL:CELL@/CELL!, TBL:N@/N!, TBL:BOOL@/BOOL!,
\ TBL:PAIR!/PAIR$ and the TBL:MAX-CELLS / TBL:PAIR-CELLS layout constants); the row
\ and field index validators and the raw byte-pointer cell accessors are
\ package-private.

require lib/errors.f

package TBL

public

2 constant PAIR-CELLS
$7FFFFFFFFFFFFFFF constant MAX-CELLS

private

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

public

: CELL-COUNT ( count count -- count ) {: rows:count fields:count :}
   rows COUNT>N 0 < if E-TBL-BOUNDS throw then
   fields COUNT>N 0 <= if E-TBL-FIELD throw then
   fields COUNT>N 0 > rows COUNT>N MAX-CELLS fields COUNT>N / > and if E-TBL-BOUNDS throw then
   rows COUNT>N fields COUNT>N * >COUNT ;

: FIELD ( ptr a count count idx idx -- ptr a ) {: tbl:ptr rows:count fields:count row:idx field:idx :}
   rows row TBL-CHECK-ROW
   fields field TBL-CHECK-FIELD
   tbl row IDX>N fields COUNT>N * field IDX>N + >OFF OFF>N cells + ;

: CELL@ ( ptr a count count idx idx -- a )
   FIELD @ ;

: CELL! ( a ptr a count count idx idx -- )
   FIELD ! ;

: N@ ( ptr a count count idx idx -- n )
   CELL@ ;

: N! ( n ptr a count count idx idx -- )
   CELL! ;

: BOOL@ ( ptr a count count idx idx -- bool )
   CELL@ ;

: BOOL! ( bool ptr a count count idx idx -- )
   CELL! ;

private

: TBL-A@ ( ptr a count count idx idx -- ptr u8 )
   CELL@ ;

: TBL-A! ( ptr u8 ptr a count count idx idx -- )
   CELL! ;

public

: PAIR! ( ptr u8 len ptr a count count idx idx -- ) {: a:ptr u:len tbl:ptr rows:count fields:count row:idx field:idx :}
   u LEN>N 0 < if E-TBL-BOUNDS throw then
   rows row TBL-CHECK-ROW
   fields field TBL-CHECK-PAIR
   a tbl rows fields row field TBL-A!
   u LEN>N tbl rows fields row field IDX>N 1 + >IDX N! ;

: PAIR$ ( ptr a count count idx idx -- ptr u8 len ) {: tbl:ptr rows:count fields:count row:idx field:idx :}
   rows row TBL-CHECK-ROW
   fields field TBL-CHECK-PAIR
   tbl rows fields row field TBL-A@
   tbl rows fields row field IDX>N 1 + >IDX N@ >LEN ;

;package
