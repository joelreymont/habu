\ Pending target selection and the one active native build action.
require src/compiler/target/model.f

package BUILD-TARGET
private

TYPED-VARIABLE PENDING RTARGET:resolved-target
TYPED-VARIABLE ACTION RTARGET:build-action
variable ACTIVE

: REFUSE-ACTIVE ( -- )
   ACTIVE @ 0<> if RTARGET:E-ACTIVE throw then ;

: END-ACTION ( -- )
   0 ACTIVE ! ;

public

: CURRENT ( -- RTARGET:resolved-target )
   ACTIVE @ 0<> if ACTION @ RTARGET:BODY-TARGET@ exit then
   PENDING @ ;

: LINUX? ( -- bool )
   CURRENT RTARGET:PROFILE@ RTARGET-PROFILE--ID:AARCH64-UNKNOWN-LINUX-GNU
   RTARGET-PROFILE--ID:EQ ;

: MACOS? ( -- bool )
   CURRENT RTARGET:PROFILE@ RTARGET-PROFILE--ID:AARCH64-APPLE-DARWIN
   RTARGET-PROFILE--ID:EQ ;

: LINUX-X86-64? ( -- bool )
   CURRENT RTARGET:PROFILE@ RTARGET-PROFILE--ID:X86-64-UNKNOWN-LINUX-GNU
   RTARGET-PROFILE--ID:EQ ;

: IDLE-CK ( -- ) REFUSE-ACTIVE ;

: ACTION@ ( -- RTARGET:build-action )
   ACTIVE @ 0= if RTARGET:E-UNSET throw then
   ACTION @ ;

: HOST! ( -- )
   REFUSE-ACTIVE
   RTARGET:HOST-TARGET PENDING ! ;

: SELECT? ( ptr u8 n -- bool )
   {: name:ptr size:n :}
   REFUSE-ACTIVE
   name size RTARGET:KNOWN? 0= if false exit then
   name size RTARGET:RESOLVE PENDING ! true ;

: WITH ( RTARGET:build-action [ RTARGET:build-action -- ] -- )
   {: action:RTARGET:build-action body :}
   REFUSE-ACTIVE
   action ACTION !
   1 ACTIVE !
   action body [: END-ACTION ;] finally ;

;package

BUILD-TARGET:HOST!
