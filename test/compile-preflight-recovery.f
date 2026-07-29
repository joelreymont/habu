\ compile-preflight-recovery.f - missing preflight remains catchable.

variable CPR-WID

: CPR-EXPECT ( n n n -- )
   {: got:n want:n code:n :}
   got want <> if code throw then ;

\ `evaluate` is the metaprogramming boundary under test.
TRUSTED: CPR-EVAL ( ptr u8 n -- n )
   [: evaluate ;] catch ;

get-current CPR-WID !
0 set-check

: CPR-HOOK ( ptr u8 n -- n )
   2drop -1 ;

' CPR-HOOK set-check

s" package CPR-NEST public : CPR-BAD ( -- ) include README.md ; ;package"
CPR-EVAL 70 1 CPR-EXPECT

get-current CPR-WID @ 2 CPR-EXPECT

LOWER-CERT-HOOK:INSTALL
: CPR-GLOBAL ( -- n ) 73 ;
package CPR-AFTER ;package
: CPR-USE ( -- n ) CPR-GLOBAL ;
CPR-USE 73 3 CPR-EXPECT

73 73 4 CPR-EXPECT
s" compile-preflight-recovery: ok" type cr
