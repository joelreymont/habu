split-str
\ rollback-cells.f - the cells an evaluate's or a REPL line's rollback puts
\ back as they were when its text began, each marked once by its owner. A
\ mark is dictionary data, so a rollback that cuts it drops it. DATA-SPACE
\ rewinds DATA to a rollback's cut.

package ROLLBACK-CELLS

\ The newest mark row, null ends. A row is two address cells: the previous row
\ and the marked cell.
PERSISTED-PTR-VARIABLE CELLS-LAST

CAST: >CELL ( ^t -- ^n )

: ROW-PREV ( ^n -- ^n ) 0 ptr-field @ ;
: ROW-CELL ( ^n -- ^n ) 1 ptr-field @ ;

public

: MARK ( ^t -- )
   >CELL {: cell:^ :}
   datum here CELL-VIEW {: row:^ :}
   here ptr-cell-mark 0 ,
   here ptr-cell-mark 0 ,
   CELLS-LAST @ row 0 ptr-field !
   cell row 1 ptr-field !
   row CELLS-LAST ! ;

: BYTES ( -- n )
   CELL CELLS-LAST @
   BEGIN dup NULL-PTR <> WHILE
      ROW-PREV swap CELL + swap
   REPEAT drop ;

\ The area holds CELLS-LAST, then each marked cell's value, newest row first.
: SAVE ( ^a -- )
   BYTE-VIEW CELL-VIEW {: area:^ :}
   CELLS-LAST @ {: first:^ :}
   first area 0 ptr-field !
   area CELL + first
   BEGIN dup NULL-PTR <> WHILE
      2dup ROW-CELL @ swap !
      ROW-PREV swap CELL + swap
   REPEAT 2drop ;

: RESTORE ( ^a -- )
   BYTE-VIEW CELL-VIEW {: area:^ :}
   area 0 ptr-field @ {: first:^ :}
   first CELLS-LAST !
   area CELL + first
   BEGIN dup NULL-PTR <> WHILE
      2dup ROW-CELL swap @ swap !
      ROW-PREV swap CELL + swap
   REPEAT 2drop ;

seal-package

;package

package DATA-SPACE
public

\ Rewinds DATA to cut, in DP-CELL's units: the address-cell rows and interned
\ texts at or above it go. The release high-water rises to the DP this lowers
\ from, so `allot` zeroes the reused space it hands out again, and the `,` run
\ ends.
: REWIND ( n -- )
   {: cut:n :}
   data-base NULL-PTR - {: base:n :}
   cut base - ADDRESS-CELLS:KEEP-BELOW
   cut INTERN:KEEP-BELOW
   data-base DP-CELL + @ base -  data-base DPHW-CELL + @ max  data-base DPHW-CELL + !
   0 data-base COMMA-RUN-CELL + !
   cut data-base DP-CELL + ! ;

seal-package

;package
