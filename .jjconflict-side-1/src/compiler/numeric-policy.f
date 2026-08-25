\ numeric-policy.f - the explicit numerical policy of a compilation.
\
\ docs/compiler-ir-design.md section 5.5. A floating-point or integer rewrite is
\ never justified by "the operation is commutative" or by an ambient fast-math
\ switch. It is justified by a policy value that the compilation carries, and
\ every rewrite rule declares which policies admit it. This file owns that value:
\ what happens on integer overflow, what floating-point execution model the
\ compiler may assume, whether a multiply-add may be contracted, which class of
\ algebraically justified rewrite is licensed, and what comparisons mean.
\
\ WHY FIVE SEPARATE FIELDS. They answer different questions and a compilation can
\ need any mixture of the answers. The rewrite licence (fast-math) says how far a
\ transformation may move the computed value. Contraction is called out on its
\ own because fusing a multiply and an add is admitted by ordinary language
\ standards while nothing else about the arithmetic is relaxed - but it does
\ change the result bits, so a bit-exact licence and allowed contraction are
\ contradictory, and that pairing is the one combination this file rejects. The
\ floating-point model describes the machine's denormal handling, which is a
\ property of the execution environment rather than of any rewrite. The
\ comparison policy says what an ordered comparison means in the presence of NaN.
\ Integer overflow is a separate axis again.
\
\ IDENTITY. CNUM:DIGEST is SHA-256 over the canonical preimage: the
\ domain-separation tag, the schema version, and one eight-byte slot per semantic
\ field in declaration order. The per-field codes are the stable wire codes; an
\ existing code may never be renumbered without bumping SCHEMA. The encoding is a
\ fixed-arity product of injective per-field codes, so two policies share a
\ preimage exactly when they are the same policy.
\
\ FORGERY. `numeric-policy` is a public family, so its generated MAKE can
\ assemble any five field values without passing through CNUM:POLICY. The words
\ that carry identity - ENCODE, DIGEST, SAME? - revalidate their input rather
\ than trust that it came from the checked constructor.

require lib/prelude.f
require lib/errors.f
require src/compiler/digest.f

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
\ DERIVE eq: SAME? below is the hand-written field-by-field identity, matching
\ the target contract, and it is the equality the digest is proved to agree with.
STRUCTURE numeric-policy 0
   FIELD overflow overflow
   FIELD float float-model
   FIELD contraction contraction
   FIELD fast-math fast-math
   FIELD compare compare
;STRUCTURE

private

\ ---- stable wire codes -------------------------------------------------------
: OVERFLOW-CODE ( CNUM:overflow -- n )
   MATCH overflow
      wrap OF 0 ENDOF
      trap OF 1 ENDOF
   ;MATCH ;

: FLOAT-CODE ( CNUM:float-model -- n )
   MATCH float-model
      ieee754        OF 0 ENDOF
      flush-denormal OF 1 ENDOF
   ;MATCH ;

: CONTRACTION-CODE ( CNUM:contraction -- n )
   MATCH contraction
      forbidden OF 0 ENDOF
      allowed   OF 1 ENDOF
   ;MATCH ;

: FAST-MATH-CODE ( CNUM:fast-math -- n )
   MATCH fast-math
      bit-exact   OF 0 ENDOF
      reassociate OF 1 ENDOF
      approximate OF 2 ENDOF
   ;MATCH ;

: COMPARE-CODE ( CNUM:compare -- n )
   MATCH compare
      ieee754-unordered OF 0 ENDOF
      total-order       OF 1 ENDOF
      assume-ordered    OF 2 ENDOF
   ;MATCH ;

\ ---- coherence rule ----------------------------------------------------------
\ Contraction changes the result bits of a multiply-add, so it cannot be allowed
\ under a licence that forbids any transformation from changing the bits.
: CONTRACT-CK ( CNUM:contraction CNUM:fast-math -- )
   {: c:contraction m:fast-math :}
   c CNUM-CONTRACTION:ALLOWED CNUM-CONTRACTION:EQ 0= if exit then
   m CNUM-FAST--MATH:BIT-EXACT CNUM-FAST--MATH:EQ if E-CNUM-CONTRACT throw then ;

\ ---- canonical preimage ------------------------------------------------------
1 constant SCHEMA
7 constant SLOTS
0 constant SLOT-TAG
1 constant SLOT-SCHEMA
2 constant SLOT-OVERFLOW
3 constant SLOT-FLOAT
4 constant SLOT-CONTRACTION
5 constant SLOT-FAST-MATH
6 constant SLOT-COMPARE

SLOTS CDIGEST:SLOT-BYTES * constant PRE-BYTES
create PRE PRE-BYTES allot

public

\ ---- construction and validation --------------------------------------------
\ The production entry point: a contradictory combination throws a named error
\ and no policy value is produced.
: POLICY ( CNUM:overflow CNUM:float-model CNUM:contraction CNUM:fast-math CNUM:compare -- CNUM:numeric-policy )
   {: o:overflow f:float-model c:contraction m:fast-math k:compare :}
   c m CONTRACT-CK
   o f c m k CNUM-NUMERIC--POLICY:MAKE ;

\ Recheck a policy that may have been assembled by the generated constructor.
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

\ The canonical preimage. The bytes live in this module and stay valid until the
\ next ENCODE call.
: ENCODE ( CNUM:numeric-policy -- ptr u8 n )
   VALIDATE CNUM-NUMERIC--POLICY:UNMAKE
   {: o:overflow f:float-model c:contraction m:fast-math k:compare :}
   CDIGEST:TAG-NUMERIC PRE SLOT-TAG CDIGEST:SLOT!
   SCHEMA PRE SLOT-SCHEMA CDIGEST:SLOT!
   o OVERFLOW-CODE PRE SLOT-OVERFLOW CDIGEST:SLOT!
   f FLOAT-CODE PRE SLOT-FLOAT CDIGEST:SLOT!
   c CONTRACTION-CODE PRE SLOT-CONTRACTION CDIGEST:SLOT!
   m FAST-MATH-CODE PRE SLOT-FAST-MATH CDIGEST:SLOT!
   k COMPARE-CODE PRE SLOT-COMPARE CDIGEST:SLOT!
   PRE PRE-BYTES ;

: DIGEST ( CNUM:numeric-policy -- CDIGEST:digest )
   ENCODE CDIGEST:COMPUTE ;

;package
