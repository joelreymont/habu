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

\ PE-OPEN and BODY-ARM are private to the checker's sealed package CHECK: user
\ source naming them is undefined.
s\" : EPV-ROWED ( -- n ) 1 ; s\q EPV-ROWED\q PE-OPEN PE-N PE-OUT PE-CLOSE"
   s" error[E-UNDEFINED]: cannot find word `PE-OPEN`" ENGINE-PRIVATE:TCE-REJECTS
s\" : EPV-ARM ( -- bool ) s\q EPV-ONE\q CHECK:BODY-ARM ;"
   s" error[E-UNDEFINED]: cannot find word `CHECK:BODY-ARM`" ENGINE-PRIVATE:TCE-REJECTS

\ The effect registrars are private: a program that could rewrite a word's
\ recorded effect would load code that underflows at run time.
s\" : EPV-TWO ( -- n ) 2 ;\ns\q EPV-TWO\q s\q -- n n\q trust-decl\n: EPV-USE ( -- n n ) EPV-TWO ;\nEPV-USE . . cr"
   s" error[E-UNDEFINED]: cannot find word `trust-decl`" ENGINE-PRIVATE:TCE-REJECTS
s\" : EPV-ONE ( -- n ) 1 ;\ns\q EPV-ONE\q s\q -- n n\q trust-raw"
   s" error[E-UNDEFINED]: cannot find word `trust-raw`" ENGINE-PRIVATE:TCE-REJECTS
s\" : EPV-ROW ( -- bool ) s\q EPV-ONE\q s\q -- n n\q CHECK:ROW-DECLARED ;"
   s" error[E-UNDEFINED]: cannot find word `CHECK:ROW-DECLARED`" ENGINE-PRIVATE:TCE-REJECTS

T-REPORT
