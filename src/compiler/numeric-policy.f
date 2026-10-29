\ numeric-policy.f - the explicit numerical policy a compilation carries. No
\ floating-point or integer rewrite is justified by commutativity or an ambient
\ fast-math switch: each rewrite rule declares which policies admit it. The five
\ fields are separate because a compilation can need any mixture of answers.

require src/lib/prelude.f
require src/lib/errors.f

package CNUM
public

\ What the compiler may assume when a signed integer operation overflows.
\ `wrap` is defined two's-complement wraparound; `trap` means the compiler owes
\ an overflow check that raises. There is deliberately no undefined-behaviour
\ variant: a policy value that licenses the optimizer to assume overflow cannot
\ happen has no place in a checked language.
ENUM overflow DERIVE eq
   wrap
   trap
;ENUM

\ The floating-point execution model the compiler may assume of the machine.
\ `ieee754` keeps denormal values; `flush-denormal` is a machine configured to
\ flush denormal inputs and results to zero.
ENUM float-model DERIVE eq
   ieee754
   flush-denormal
;ENUM

\ May a multiply followed by an add be contracted into one fused operation,
\ dropping the intermediate rounding?
ENUM contraction DERIVE eq
   forbidden
   allowed
;ENUM

\ The strongest class of algebraically justified rewrite the compilation admits,
\ from strongest guarantee to weakest:
\   bit-exact   - no transformation may change the computed bits;
\   reassociate - exact-arithmetic identities (associativity, commutativity,
\                 distribution) may be applied, so summation order may change;
\   approximate - reciprocal, reciprocal-square-root and transcendental
\                 substitutions with a bounded error may be applied as well.
ENUM fast-math DERIVE eq
   bit-exact
   reassociate
   approximate
;ENUM

\ What an ordered floating-point comparison means.
\   ieee754-unordered - IEEE-754 semantics: a NaN operand makes the comparison
\                       unordered, so x = x is false for NaN;
\   total-order       - the IEEE-754 totalOrder predicate: NaN and signed zero
\                       are ordered, so comparison is a total order;
\   assume-ordered    - the compiler may assume no operand is NaN.
ENUM compare DERIVE eq
   ieee754-unordered
   total-order
   assume-ordered
;ENUM

\ The policy. Fields in declaration order, deepest stack field first. It does not
\ DERIVE eq: SAME? below compares its fields, as in the target contract.
STRUCTURE numeric-policy
   FIELD overflow overflow
   FIELD float float-model
   FIELD contraction contraction
   FIELD fast-math fast-math
   FIELD compare compare
;STRUCTURE

private

\ ---- coherence rule ----------------------------------------------------------
\ Contraction changes the result bits of a multiply-add, so it cannot be allowed
\ under a licence that forbids any transformation from changing the bits.
: CONTRACT-CK ( CNUM:contraction CNUM:fast-math -- )
   {: c:contraction m:fast-math :}
   c CNUM-CONTRACTION:ALLOWED CNUM-CONTRACTION:EQ 0= if exit then
   m CNUM-FAST--MATH:BIT-EXACT CNUM-FAST--MATH:EQ if E-CNUM-CONTRACT throw then ;

public

\ ---- construction and validation --------------------------------------------
\ The production entry point: a contradictory combination throws a named error
\ and no policy value is produced.
: POLICY ( CNUM:overflow CNUM:float-model CNUM:contraction CNUM:fast-math CNUM:compare -- CNUM:numeric-policy )
   {: o:overflow f:float-model c:contraction m:fast-math k:compare :}
   c m CONTRACT-CK
   o f c m k CNUM-NUMERIC--POLICY:MAKE ;

\ `numeric-policy` is a public family, so its generated MAKE can assemble any
\ five field values without POLICY; SAME? revalidates rather than trusting it.
: VALIDATE ( CNUM:numeric-policy -- CNUM:numeric-policy )
   CNUM-NUMERIC--POLICY:UNMAKE POLICY ;

\ ---- field readers -----------------------------------------------------------
: OVERFLOW@ ( CNUM:numeric-policy -- CNUM:overflow )
   CNUM-NUMERIC--POLICY:UNMAKE drop drop drop drop ;

: FLOAT@ ( CNUM:numeric-policy -- CNUM:float-model )
   CNUM-NUMERIC--POLICY:UNMAKE drop drop drop nip ;

: CONTRACTION@ ( CNUM:numeric-policy -- CNUM:contraction )
   CNUM-NUMERIC--POLICY:UNMAKE drop drop nip nip ;

: FAST-MATH@ ( CNUM:numeric-policy -- CNUM:fast-math )
   CNUM-NUMERIC--POLICY:UNMAKE drop nip nip nip ;

: COMPARE@ ( CNUM:numeric-policy -- CNUM:compare )
   CNUM-NUMERIC--POLICY:UNMAKE nip nip nip nip ;

\ ---- identity ----------------------------------------------------------------
: SAME? ( CNUM:numeric-policy CNUM:numeric-policy -- bool )
   VALIDATE CNUM-NUMERIC--POLICY:UNMAKE
   {: yo:overflow yf:float-model yc:contraction ym:fast-math yk:compare :}
   VALIDATE CNUM-NUMERIC--POLICY:UNMAKE
   {: xo:overflow xf:float-model xc:contraction xm:fast-math xk:compare :}
   xo yo CNUM-OVERFLOW:EQ
   xf yf CNUM-FLOAT--MODEL:EQ and
   xc yc CNUM-CONTRACTION:EQ and
   xm ym CNUM-FAST--MATH:EQ and
   xk yk CNUM-COMPARE:EQ and ;

;package
