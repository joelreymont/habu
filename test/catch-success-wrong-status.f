\ A plain zero cannot prove that this caught reader returned normally.
1 set-tier
package CATCH-REFUSAL
private
variable THROW-MODE
: NEW-VALUE ( -- n ) THROW-MODE @ 0<> if 84 throw then 31 ;
: READER ( n -- n ) drop NEW-VALUE ;
public
: WRONG-STATUS ( n -- n )
   [: READER ;] catch {: value code:n :}
   0 0= if value 1+ else 0 then ;
;package

package CATCH-REFUSAL
public
: EXERCISE ( -- ) 1 WRONG-STATUS drop ;
;package
CATCH-REFUSAL:EXERCISE
