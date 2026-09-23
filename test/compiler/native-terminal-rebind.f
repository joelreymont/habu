\ Run only in a child: source replacements are ordinary calls, even at global
\ scope. The terminal fact requires the primitive's engine-text entry.
1 set-tier
require lib/test.f
undefine throw
: throw ( n -- n ) 1+ ;
undefine die
: die ( n -- n ) 2 + ;
package TERMINAL-REBIND-TEST
: ONE ( n -- n ) throw ;
: TWO ( n -- n ) die ;
: RUN ( -- ) T-RESET 41 ONE 42 T= 40 TWO 42 T= T-REPORT ;
RUN
;package
