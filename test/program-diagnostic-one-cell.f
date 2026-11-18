\ A two-cell value met where a word's effect makes a variable one cell: each
\ record names the variable, that word and the value's cells.
package PROGRAM-DIAG-CELL
public
: START ( -- ) CHECK:MULTI-ERR-BEGIN ;
: FINISH ( -- ) CHECK:MULTI-ERR-END 3 <> if s" one-cell rejection count" 1 die then ;
;package

STRUCTURE pr FIELD x n FIELD y n ;STRUCTURE
: AP ( [ -- t ] -- [ -- t ] ) ;
: W ( -- pr ) 1 2 PR:MAKE ;
defer D ( t -- t )
: G ( ^t -- t ) @ ;

PROGRAM-DIAG-CELL:START
: QUOT-ROW ( -- ) ['] W AP drop ;
: DEFER-CALL ( -- pr ) 1 2 PR:MAKE D ;
: BODY-FETCH ( ^pr -- pr ) G ;
PROGRAM-DIAG-CELL:FINISH
s" ok" type cr
