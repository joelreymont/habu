\ table.f - binary search over sorted tables of Unicode rows. A row is a
\ fixed number of cells whose first cell is its sort key; a range row's
\ first two cells are an inclusive range, disjoint from every other row's.
\
\ STORAGE CLASS. No state; any task may call.

require src/lib/sort.f

package UNICODE-TABLE

private

: ROW ( ^n n n -- ^n )
   {: rows:^ idx:n width:n :}
   rows idx width * cells + ;

\ The last row whose first cell is at most cp, and whether there is one.
: AT-MOST ( n ^n n n -- ^n bool )
   {: cp:n rows:^ count:n width:n :}
   0 count [: rows swap width ROW @ cp <= ;] SORT:PARTITION-POINT
   {: at:n :}
   at 0= if rows false exit then
   rows at 1- width ROW true ;

public

\ The row of width cells whose range holds cp, and true; else any
\ address and false.
: RANGE-ROW ( n ^n n n -- ^n bool )
   {: cp:n rows:^ count:n width:n :}
   cp rows count width AT-MOST 0= if false exit then
   dup cell+ @ cp >= ;

\ Whether a two-cell range holds cp.
: IN-RANGES? ( n ^n n -- bool )
   2 RANGE-ROW nip ;

\ The second cell of the two-cell entry whose first cell is cp, and
\ true; else 0 and false.
: PAIR ( n ^n n -- n bool )
   {: cp:n pairs:^ count:n :}
   cp pairs count 2 AT-MOST 0= if drop 0 false exit then
   {: pair:^ :}
   pair @ cp <> if 0 false exit then
   pair cell+ @ true ;

;package
