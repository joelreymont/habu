\ The failure arm leaves a stale value at the live join.
1 set-tier
package CATCH-REFUSAL
private
variable THROW-MODE
: NEW-VALUE ( -- n ) THROW-MODE @ 0<> if 84 throw then 31 ;
: READER ( n -- n ) drop NEW-VALUE ;
public
: UNSAFE-JOIN ( n -- n )
   [: READER ;] catch {: code:n :}
   code 0= if 1+ else then
   1+ ;
;package

package CATCH-REFUSAL
public
: EXERCISE ( -- ) 1 UNSAFE-JOIN drop ;
;package
CATCH-REFUSAL:EXERCISE
