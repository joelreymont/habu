\ checker-redefinition-order-suite.f - which record of a redefined word its
\ callers are checked against: the newest wins, and a deletion is an absence.

require src/lib/test.f

package REDEFINITION-ORDER-TEST

T-RESET
variable TC                    \ last caught throw code

\ Each caller has its own name, so a refusal is the checker's and never
\ a duplicate definition.
: SCX-DEF1 ( -- ) s" : SCXA ( n -- n ) ;" evaluate-closed ;
: SCX-UNDEF ( -- ) s" undefine SCXA" evaluate-closed ;
: SCX-DEF2 ( -- ) s" : SCXA ( n n -- n ) drop ;" evaluate-closed ;
: SCX-DEF3 ( -- ) s" undefine SCXA : SCXA ( n n n -- n ) drop drop ;" evaluate-closed ;
: SCX-DEF4 ( -- ) s" : SCXA ( n n n -- n ) drop drop ;" evaluate-closed ;
: SCX-USE0 ( -- ) s" : SCXU0 ( n -- n ) SCXA ;" evaluate-closed ;
: SCX-USE1 ( -- ) s" : SCXU1 ( n -- n ) SCXA ;" evaluate-closed ;
: SCX-USE2 ( -- ) s" : SCXU2 ( n -- n ) SCXA ;" evaluate-closed ;
: SCX-USE3 ( -- ) s" : SCXU3 ( n n -- n ) SCXA ;" evaluate-closed ;
: SCX-USE4 ( -- ) s" : SCXU4 ( n -- n ) SCXA ;" evaluate-closed ;
: SCX-USE5 ( -- ) s" : SCXU5 ( n n n -- n ) SCXA ;" evaluate-closed ;
: SCX-USE6 ( -- ) s" : SCXU6 ( n n n -- n ) SCXA ;" evaluate-closed ;
: SCX-USE7 ( -- ) s" : SCXU7 ( n n n -- n ) SCXA ;" evaluate-closed ;

s" a caller of a word not yet defined is refused" T-LABEL
' SCX-USE0 catch TC !   TC @ 0 <> TTRUE
' SCX-DEF1 catch TC !   TC @ 0 T=
s" a caller is held to the first effect" T-LABEL
' SCX-USE1 catch TC !   TC @ 0 T=
' SCX-UNDEF catch TC !  TC @ 0 T=
s" a deleted word has no effect left to call" T-LABEL
' SCX-USE2 catch TC !   TC @ 0 <> TTRUE
' SCX-DEF2 catch TC !   TC @ 0 T=
s" a redefinition's effect replaces the first one" T-LABEL
' SCX-USE3 catch TC !   TC @ 0 T=
' SCX-USE4 catch TC !   TC @ 0 <> TTRUE
' SCX-DEF3 catch TC !   TC @ 0 T=
s" a deletion and redefinition on one line take the newest effect" T-LABEL
' SCX-USE5 catch TC !   TC @ 0 T=
' SCX-UNDEF catch TC !  TC @ 0 T=
' SCX-USE6 catch TC !   TC @ 0 <> TTRUE
' SCX-DEF4 catch TC !   TC @ 0 T=
s" a definition after a deletion answers again" T-LABEL
' SCX-USE7 catch TC !   TC @ 0 T=

\ The caller's arm leaves one value more than its fall-through, so it certifies
\ only while SCXT ends the path.
: SCX-CTLDEF ( -- ) s" : SCXT ( n -- n ) 7101 throw ;" evaluate-closed ;
: SCX-CTLREDEF ( -- ) s" undefine SCXT : SCXT ( n -- n ) ;" evaluate-closed ;
: SCX-CTLUSE1 ( -- ) s" : SCXTU1 ( n -- n ) dup 0 = if 5 SCXT then ;" evaluate-closed ;
: SCX-CTLUSE2 ( -- ) s" : SCXTU2 ( n -- n ) dup 0 = if 5 SCXT then ;" evaluate-closed ;

' SCX-CTLDEF catch TC !   TC @ 0 T=
s" an arm ending in a word whose body throws need not reach the join" T-LABEL
' SCX-CTLUSE1 catch TC !  TC @ 0 T=
' SCX-CTLREDEF catch TC !  TC @ 0 T=
s" after a redefinition that returns, the same arm is refused" T-LABEL
' SCX-CTLUSE2 catch TC !  TC @ 0 <> TTRUE

T-REPORT

;package
