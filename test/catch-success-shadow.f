\ A package word with a familiar zero-test name has no core zero relation.
package CS-SHADOW
public
: 0= ( n -- bool ) drop true ;
;package

package CATCH-REFUSAL
private
variable THROW-MODE
: NEW-VALUE ( -- n ) THROW-MODE @ 0<> if 84 throw then 31 ;
: READER ( n -- n ) drop NEW-VALUE ;
public
: SHADOW-GUARD ( n -- n )
   [: READER ;] catch {: value code:n :}
   code CS-SHADOW:0= if value 1+ else 0 then ;
: EXERCISE ( -- ) 1 SHADOW-GUARD drop ;
;package

CATCH-REFUSAL:EXERCISE
