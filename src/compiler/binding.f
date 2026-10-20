\ binding.f - the compiler target and numerical policy bound together.
\
\ docs/compiler-ir-design.md sections 5.4 and 5.5. A target contract says what
\ machine the compilation is for; a numerical policy says what the compiler may
\ assume about arithmetic. Neither is complete on its own, and some combinations
\ of the two are not realisable even though each side is coherent by itself. This
\ file binds the pair and rejects those combinations.
\
\ WHAT THE BINDING VALIDATOR OWNS. Only the facts that need both halves to
\ decide. Today that is one rule: a policy may not allow a multiply-add to be
\ contracted onto a target that has no fused multiply-add. Everything decidable
\ from one half alone stays with that half, and both halves are revalidated here
\ so a forged component cannot enter a binding.
\

require src/lib/prelude.f
require src/lib/errors.f
require src/compiler/target.f
require src/compiler/numeric-policy.f

package CBIND
public

\ The bound pair. Fields in declaration order, deepest stack field first: the
\ target contract, then the numerical policy.
STRUCTURE binding
   FIELD target CTARGET:contract
   FIELD numeric CNUM:numeric-policy
;STRUCTURE

private

\ ---- coherence rule ----------------------------------------------------------
\ Contraction needs a fused multiply-add in the machine. A target without that
\ feature cannot honour a policy that licenses the fusion.
: FMA-CK ( CNUM:contraction CTARGET:features -- )
   {: c:CNUM:contraction f:CTARGET:features :}
   c CNUM-CONTRACTION:ALLOWED CNUM-CONTRACTION:EQ 0= if exit then
   f CTARGET:F-FP CTARGET:HAS? 0= if E-CBIND-CONTRACT throw then ;

public

\ ---- construction and validation --------------------------------------------
\ The production entry point. Both halves are revalidated and the pair rule is
\ applied; an unrealisable pairing throws a named error and no binding is made.
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
\ Structural identity, delegated to each half's own field-by-field comparison.
: SAME? ( CBIND:binding CBIND:binding -- bool )
   CBIND-BINDING:UNMAKE
   {: yt:CTARGET:contract yn:CNUM:numeric-policy :}
   CBIND-BINDING:UNMAKE
   {: xt:CTARGET:contract xn:CNUM:numeric-policy :}
   xn yn CNUM:SAME? xt yt CTARGET:SAME? and ;

;package
