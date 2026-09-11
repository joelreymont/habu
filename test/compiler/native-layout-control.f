\ The bind/grow control block contains numeric offset, capacity and live count.
package LAYOUT-CONTROL-TEST
create CONTROL 3 cells allot
: CONTROL-CELL ( n -- ptr n ) cells CONTROL + ;
: ROWS ( -- ptr n ) 0 CONTROL-CELL dup @ + ;
: CHECK ( -- )
   0 0 CONTROL-CELL ! 0 1 CONTROL-CELL ! 0 2 CONTROL-CELL !
   2 0 CONTROL-CELL 1 LDEFER-BIND
   1 CONTROL-CELL @ 2 <> 2 CONTROL-CELL @ 2 <> or if
      s" layout bind stored incorrect control values" 76 die then
   37 ROWS !
   5 0 CONTROL-CELL 1 LDEFER-GROW
   ROWS @ 37 <> ROWS 4 cells + @ 0<> or if
      s" layout grow lost cells or failed to clear new storage" 76 die then
   88 ROWS cell + !
   1 0 CONTROL-CELL 1 LDEFER-GROW
   3 0 CONTROL-CELL 1 LDEFER-GROW
   ROWS @ 37 <> ROWS cell + @ 0<> or 2 CONTROL-CELL @ 3 <> or if
      s" layout regrow failed to clear newly exposed cells" 76 die then
   s" BAD-BIND ( ptr bool -- ) 1 swap 1 LDEFER-BIND" CHECK-CANDIDATE! 0<> if
      s" layout bind accepted boolean control cells" 76 die then
   s" BAD-GROW ( ptr r -- ) 1 swap 1 LDEFER-GROW" CHECK-CANDIDATE! 0<> if
      s" layout grow accepted floating control cells" 76 die then ;
CHECK
;package
