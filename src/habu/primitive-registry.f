\ Build-side primitive metadata. Names are offsets, so growing either buffer
\ cannot invalidate an earlier row. The emitted dictionary format is unchanged.
require src/core/layout-buffer.f
require src/core/bytes.f
require src/core/roles.f

package ENGINE-PRIMS

6 constant ROW-CELLS
$7FFFFFFFFFFFFFFF constant MAX-N
\ Match the generated storage accessors' size and index refusals.
7121 constant E-SIZE
7122 constant E-INDEX
DYNAMIC-BUFFER ROWS n
DYNAMIC-BUFFER NAMES n
variable USED
variable NAME-BYTES

: FIELD ( n n -- ptr n ) {: row:n field:n :}
   row 0 < row USED @ >= or if E-INDEX throw then
   row ROW-CELLS * field + ROWS ;

: RESERVE ( n -- ) {: size:n :}
   size 0 <= if E-SIZE throw then
   USED @ MAX-N CELL / ROW-CELLS / >= if E-SIZE throw then
   size MAX-N CELL 1- - NAME-BYTES @ - > if E-SIZE throw then
   USED @ 1+ ROW-CELLS * ROWS-RESERVE
   NAME-BYTES @ size + CELL 1- + CELL / NAMES-RESERVE ;

public

: COUNT ( -- n ) USED @ ;
: RESET ( -- ) 0 USED ! 0 NAME-BYTES ! ;
: RELEASE ( -- ) RESET ROWS-RELEASE NAMES-RELEASE ;

: ADD ( ptr u8 n label label -- n ) {: name:ptr size:n first:label last:label :}
   size RESERVE
   USED @ {: row:n :}
   row ROW-CELLS * ROWS {: dst:ptr :}
   first LABEL>N dst ! last LABEL>N dst cell+ !
   size dst 2 cells + ! NAME-BYTES @ dst 3 cells + !
   -1 dst 4 cells + ! 0 dst 5 cells + !
   name 0 NAMES BYTE-VIEW NAME-BYTES @ + size BYTE-COPY
   NAME-BYTES @ size + NAME-BYTES !
   row 1+ USED !
   row ;

: FIRST-LABEL ( n -- label ) 0 FIELD @ >LABEL ;
: LAST-LABEL ( n -- label ) 1 FIELD @ >LABEL ;
: NAME-LEN ( n -- n ) 2 FIELD @ ;
: NAME$ ( n -- ptr u8 n ) {: row:n :}
   0 NAMES BYTE-VIEW row 3 FIELD @ + row NAME-LEN ;
: NAME-LABEL ( n -- label ) 4 FIELD @ >LABEL ;
: NAME-LABEL! ( label n -- ) swap LABEL>N swap 4 FIELD ! ;
: WID ( n -- n ) 5 FIELD @ ;
: WID! ( n n -- ) 5 FIELD ! ;

;package
