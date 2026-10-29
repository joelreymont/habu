\ binding.f - a compiler target contract and a numerical policy bound together.
\
\ Only rules that need both halves live here: a policy may not allow multiply-add
\ contraction on a target without a fused multiply-add (E-CBIND-CONTRACT). BIND
\ revalidates both halves, so a forged component cannot enter a binding.

require src/lib/prelude.f
require src/lib/errors.f
require src/compiler/target.f
require src/compiler/numeric-policy.f

package CBIND
public

STRUCTURE binding
   FIELD target CTARGET:contract
   FIELD numeric CNUM:numeric-policy
;STRUCTURE

private

\ ---- coherence rule ----------------------------------------------------------
: FMA-CK ( CNUM:contraction CTARGET:features -- )
   {: c:CNUM:contraction f:CTARGET:features :}
   c CNUM-CONTRACTION:ALLOWED CNUM-CONTRACTION:EQ 0= if exit then
   f CTARGET:F-FP CTARGET:HAS? 0= if E-CBIND-CONTRACT throw then ;

public

\ ---- construction and validation --------------------------------------------
: BIND ( CTARGET:contract CNUM:numeric-policy -- CBIND:binding )
   CNUM:VALIDATE {: numeric:CNUM:numeric-policy :}
   CTARGET:VALIDATE {: target:CTARGET:contract :}
   numeric CNUM:CONTRACTION@ target CTARGET:FEATURES@ FMA-CK
   target numeric CBIND-BINDING:MAKE ;

\ Recheck a binding that may have been assembled by the generated constructor.
: VALIDATE ( CBIND:binding -- CBIND:binding )
   CBIND-BINDING:UNMAKE BIND ;

\ ---- field readers -----------------------------------------------------------
: TARGET@ ( CBIND:binding -- CTARGET:contract )
   CBIND-BINDING:UNMAKE
   CNUM-NUMERIC--POLICY:UNMAKE drop drop drop drop drop ;

: POLICY@ ( CBIND:binding -- CNUM:numeric-policy )
   CBIND-BINDING:UNMAKE
   {: target:CTARGET:contract numeric:CNUM:numeric-policy :}
   numeric ;

\ ---- identity ----------------------------------------------------------------
: SAME? ( CBIND:binding CBIND:binding -- bool )
   CBIND-BINDING:UNMAKE
   {: yt:CTARGET:contract yn:CNUM:numeric-policy :}
   CBIND-BINDING:UNMAKE
   {: xt:CTARGET:contract xn:CNUM:numeric-policy :}
   xn yn CNUM:SAME? xt yt CTARGET:SAME? and ;

;package
