\ engine-private-test.f - a program cannot name the engine's private words.
\ Each case evaluates one source string, so the record captured is that string's.

require src/lib/test.f

package ENGINE-PRIVATE
private

T-RESET

TYPED-VARIABLE TCE-A ^u8   variable TCE-U
: TCE-GO ( -- )  TCE-A @ TCE-U @ evaluate-closed ;

public

\ TCE-REJECTS asserts the string stops with the checker's status after a record
\ that holds the text given.
: TCE-REJECTS ( ^u8 n ^u8 n -- ) {: wa:^ wu:n :}
   TCE-U ! TCE-A !  REJECTION:CAPTURE  [: TCE-GO ;] catch  REJECTION:BUFFER-OFF
   CHECKER-REJECT-RC T=  REJECTION:BUFFER$ wa wu CONTAINS? TTRUE ;

;package

\ PE-OPEN is private to the checker's sealed package CHECK: user source naming
\ it is undefined.
s\" : EPV-ROWED ( -- n ) 1 ; s\q EPV-ROWED\q PE-OPEN PE-N PE-OUT PE-CLOSE"
   s" error[E-UNDEFINED]: cannot find word `PE-OPEN`" ENGINE-PRIVATE:TCE-REJECTS

T-REPORT
