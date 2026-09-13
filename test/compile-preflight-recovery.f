\ compile-preflight-recovery.f - missing preflight remains catchable.

variable CPR-WID
variable CPR-DEPTH

: CPR-EXPECT ( n n n -- )
   {: got:n want:n code:n :}
   got want <> if code throw then ;

\ Catch a zero-input action: catching evaluate directly restores its source
\ arguments on failure, which would contradict the helper's single-result effect.
package CPR-EVAL
PTR-VARIABLE SOURCE
variable LENGTH

TRUSTED: ACT ( -- ) SOURCE @ LENGTH @ evaluate ;

public
: RUN ( ptr u8 n -- n )
   LENGTH ! SOURCE ! [: ACT ;] catch ;
;package

get-current CPR-WID !
\ Disabling checking and installing CPR-HOOK are the unchecked-region boundary;
\ TYPE-FIXES-PLAN item 26 replaces both with NO-TYPE-CHECK.
0 set-check

: CPR-HOOK ( ptr u8 n -- n )
   2drop -1 ;

' CPR-HOOK set-check

depth CPR-DEPTH !
s" package CPR-NEST public : CPR-BAD ( -- ) include README.md ; ;package"
CPR-EVAL:RUN 70 1 CPR-EXPECT
depth CPR-DEPTH @ 5 CPR-EXPECT

get-current CPR-WID @ 2 CPR-EXPECT

LOWER-CERT-HOOK:INSTALL
: CPR-GLOBAL ( -- n ) 73 ;
package CPR-AFTER ;package
: CPR-USE ( -- n ) CPR-GLOBAL ;
CPR-USE 73 3 CPR-EXPECT

73 73 4 CPR-EXPECT
s" compile-preflight-recovery: ok" type cr
