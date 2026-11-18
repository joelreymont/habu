split-str
\ A two-cell value met where a word's effect makes a variable one cell or steps
\ a pointer to it: each record names the variable, that word and the value.
package PROGRAM-DIAG-CELL
public
: START ( -- ) CHECK:MULTI-ERR-BEGIN ;
: FINISH ( -- ) CHECK:MULTI-ERR-END 5 <> if s" one-cell rejection count" 1 die then ;
;package

STRUCTURE pr FIELD x n FIELD y n ;STRUCTURE
: AP ( [ -- t ] -- [ -- t ] ) ;
: W ( -- pr ) 1 2 PR:MAKE ;
defer D ( t -- t )
: G ( ^t -- t ) @ ;
STRUCTURE box<a> DERIVE addr FIELD item a FIELD rest n ;STRUCTURE
: IN-BOX ( box<t> -- box<t> ) ;
: STEP ( ^t -- ^t ) 8 + ;

PROGRAM-DIAG-CELL:START
: QUOT-ROW ( -- ) ['] W AP drop ;
: DEFER-CALL ( -- pr ) 1 2 PR:MAKE D ;
: BODY-FETCH ( ^pr -- pr ) G ;
: BOX-WIDTH ( box<pr> -- box<pr> ) IN-BOX ;
: PTR-STEP ( ^pr -- ^pr ) STEP ;
PROGRAM-DIAG-CELL:FINISH
s" ok" type cr
