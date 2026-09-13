\ The saved installer must write the deferred word's relocated DATA cell.
package DEFER-IMAGE-SUBJECT
private

defer ACTION ( n -- n )

: INCREMENT ( n -- n ) 1+ ;
: DOUBLE ( n -- n ) 2 * ;
: EQ! ( n n -- ) <> if 79 throw then ;

public

: INSTALL ( [ n -- n ] -- ) is ACTION ;
: CALL ( n -- n ) ACTION ;

: FIRST ( -- ) ['] INCREMENT INSTALL ;
: SECOND ( -- ) ['] DOUBLE INSTALL ;

: CHECK-REASSIGNMENT ( -- )
   64 0 do
      FIRST i CALL i 1+ EQ!
      SECOND i CALL i 2 * EQ!
   loop ;

CHECK-REASSIGNMENT
FIRST
s" defer-source: ok" type cr
;package
