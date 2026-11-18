split-str
require src/core/exit-status.f
require src/compiler/target/model.f
require src/compiler/numeric-policy.f
require src/compiler/binding.f

package RTARGET
private

\ The target's name when CORE supports it, else empty.
: SUPPORTED-LABEL ( profile-id -- str )
   {: p:profile-id :}
   p dup DEFAULT-FEATURES RTARGET-RESOLVED--TARGET:MAKE CORE
   MATCH core-result
      supported OF drop p LABEL ENDOF
      unsupported OF s" " ENDOF
   ;MATCH ;

\ The target came from a caller at run time, so the rejection has no source token.
: UNSUPPORTED ( resolved-target -- )
   {: target:resolved-target :}
   s" E-UNSUPPORTED-TARGET" REJECTION:OPEN
   s" Habu has no native binding for " REJECTION:EMIT-STR
   96 REJECTION:EMIT1  target PROFILE$ REJECTION:EMIT-STR  96 REJECTION:EMIT1
   s" supported" REJECTION:NOTE
   false [: SUPPORTED-LABEL SAY-ITEM ;] EACH-PROFILE drop
   REJECTION:HELP  s" build for one of the supported targets" REJECTION:EMIT-STR
   EXIT-STATUS:REJECTED REJECTION:STOP ;

public

: NATIVE-BINDING ( resolved-target -- CBIND:binding )
   {: target:resolved-target :}
   target CORE MATCH core-result
      supported OF
         CNUM-OVERFLOW:WRAP CNUM-FLOAT--MODEL:IEEE754
         CNUM-CONTRACTION:FORBIDDEN CNUM-FAST--MATH:BIT-EXACT
         CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY CBIND:BIND
      ENDOF
      unsupported OF target UNSUPPORTED ENDOF
   ;MATCH ;

;package
