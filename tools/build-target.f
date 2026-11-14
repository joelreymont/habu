\ Pending target selection and the one active native build action.
require src/compiler/target/model.f

package BUILD-TARGET
private

TYPED-VARIABLE PENDING RTARGET:resolved-target
TYPED-VARIABLE ACTION RTARGET:build-action
variable ACTIVE

: REJECT-ACTIVE ( -- )
   ACTIVE @ 0= if exit then
   s" E-BUILD-ACTIVE" REJECTION:OPEN
   s" the build target cannot change while `BUILD-TARGET:WITH` runs its action"
   REJECTION:EMIT-STR
   REJECTION:HELP
   s" select the target before `BUILD-TARGET:WITH` or after its action returns"
   REJECTION:EMIT-STR
   CHECKER-REJECT-RC REJECTION:STOP ;

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

: HOST! ( -- )
   REJECT-ACTIVE
   RTARGET:HOST-TARGET PENDING ! ;

: SELECT? ( ptr u8 n -- bool )
   {: name:ptr size:n :}
   REJECT-ACTIVE
   name size RTARGET:KNOWN? 0= if false exit then
   name size RTARGET:RESOLVE PENDING ! true ;

: WITH ( RTARGET:build-action [ RTARGET:build-action -- ] -- )
   {: action:RTARGET:build-action body :}
   REJECT-ACTIVE
   action ACTION !
   1 ACTIVE !
   action body [: END-ACTION ;] finally ;

;package

BUILD-TARGET:HOST!
