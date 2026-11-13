\ rollback-cells.f - the cells an evaluate's or a REPL line's rollback puts
\ back as they were when its text began, each marked once by its owner. A
\ mark is dictionary data, so a rollback that cuts it drops it.

package ROLLBACK-CELLS

\ The newest mark row, null ends. A row is two address cells: the previous row
\ and the marked cell.
PERSISTED-PTR-VARIABLE CELLS-LAST

CAST: >CELL ( ptr t -- ptr n )

: ROW-PREV ( ptr n -- ptr n ) 0 ptr-field @ ;
: ROW-CELL ( ptr n -- ptr n ) 1 ptr-field @ ;

public

: MARK ( ptr t -- )
   >CELL {: cell:ptr :}
   align here CELL-VIEW {: row:ptr :}
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
: SAVE ( ptr a -- )
   BYTE-VIEW CELL-VIEW {: area:ptr :}
   CELLS-LAST @ {: first:ptr :}
   first area 0 ptr-field !
   area CELL + first
   BEGIN dup NULL-PTR <> WHILE
      2dup ROW-CELL @ swap !
      ROW-PREV swap CELL + swap
   REPEAT 2drop ;

: RESTORE ( ptr a -- )
   BYTE-VIEW CELL-VIEW {: area:ptr :}
   area 0 ptr-field @ {: first:ptr :}
   first CELLS-LAST !
   area CELL + first
   BEGIN dup NULL-PTR <> WHILE
      2dup ROW-CELL swap @ swap !
      ROW-PREV swap CELL + swap
   REPEAT 2drop ;

seal-package

;package
