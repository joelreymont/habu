\ engine-private-test.f - a program cannot name the engine's private words.
\ Each case evaluates one source string, so the code returned is that string's.

require src/lib/test.f

package ENGINE-PRIVATE
private

T-RESET

\ TCE-CATCH answers the string's throw code, 0 when accepted.
TYPED-VARIABLE TCE-A ^u8   variable TCE-U
: TCE-GO ( -- )  TCE-A @ TCE-U @ evaluate-closed ;

public

: TCE-CATCH ( ^u8 n -- n )  TCE-U ! TCE-A !  [: TCE-GO ;] catch ;

\ The code lives in the package because a global `E-*` constant is
\ src/lib/error.f's surface alone.
70 constant E-REJECT            \ E-UNDEFINED / checker rejection

;package

\ PE-OPEN is private to the checker's sealed package CHECK: user source naming
\ it is undefined.
s\" : EPV-ROWED ( -- n ) 1 ; s\q EPV-ROWED\q PE-OPEN PE-N PE-OUT PE-CLOSE" ENGINE-PRIVATE:TCE-CATCH ENGINE-PRIVATE:E-REJECT T=

T-REPORT
