\ Native Habu binding for a resolved profile. NABI retains routine constructors.
require src/compiler/target/model.f
require src/compiler/numeric-policy.f
require src/compiler/binding.f

package RTARGET
public

: NATIVE-BINDING ( resolved-target -- CBIND:binding )
   CORE MATCH core-result
      supported OF
         CNUM-OVERFLOW:WRAP CNUM-FLOAT--MODEL:IEEE754
         CNUM-CONTRACTION:FORBIDDEN CNUM-FAST--MATH:BIT-EXACT
         CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY CBIND:BIND
      ENDOF
      unsupported OF E-UNSUPPORTED throw ENDOF
   ;MATCH ;

;package
