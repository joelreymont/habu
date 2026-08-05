\ binding.f - the compiler target and numerical policy bound into one identity.
\
\ docs/compiler-ir-design.md sections 5.4 and 5.5. A target contract says what
\ machine the compilation is for; a numerical policy says what the compiler may
\ assume about arithmetic. Neither is complete on its own, and some combinations
\ of the two are not realisable even though each side is coherent by itself. This
\ file binds the pair, rejects those combinations, and gives the pair one digest
\ - the value a later stage records so that "this table was built under exactly
\ this policy" is a checkable statement rather than a comment.
\
\ WHAT THE BINDING VALIDATOR OWNS. Only the facts that need both halves to
\ decide. Today that is one rule: a policy may not allow a multiply-add to be
\ contracted onto a target that has no fused multiply-add. Everything decidable
\ from one half alone stays with that half, and both halves are revalidated here
\ so a forged component cannot enter a binding.
\
\ IDENTITY. CBIND:DIGEST is SHA-256 over a preimage that begins with the binding
\ tag and schema version and then carries the COMPLETE canonical preimage of the
\ target contract followed by the complete canonical preimage of the numerical
\ policy - each including its own tag and schema version. Composing the preimages
\ rather than the component digests has two consequences worth stating. First,
\ the binding digest depends directly on every semantic field of both halves, so
\ its injectivity does not rest on the components' hashes being collision-free,
\ only on the encoding being injective. Second, a schema change on either side
\ flows into the binding digest automatically: no one has to remember to bump a
\ second version number, and no component field can be added that the binding
\ digest fails to see.

require lib/prelude.f
require lib/errors.f
require src/compiler/digest.f
require src/compiler/target.f
require src/compiler/numeric-policy.f

package CBIND
public

\ The bound pair. Fields in declaration order, deepest stack field first: the
\ target contract, then the numerical policy.
STRUCTURE binding 0
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

\ ---- canonical preimage ------------------------------------------------------
1 constant SCHEMA
0 constant SLOT-TAG
1 constant SLOT-SCHEMA
2 constant SLOT-HEAD          \ first slot a component preimage is copied into

\ Exactly the tag, the schema version, and the two component preimages as they
\ stand today. No slack: if a component schema grows a slot, APPEND throws
\ E-CBIND-PREIMAGE and every binding test goes red, which is the signal to widen
\ this alongside the component. Slack here would let a component grow silently.
16 constant SLOT-CAP

SLOT-CAP CDIGEST:SLOT-BYTES * constant PRE-BYTES
create PRE PRE-BYTES allot

\ Copy a component's whole canonical preimage into the binding preimage and
\ report the next free slot.
: APPEND ( ptr u8 n n -- n )
   {: src:ptr len:n at:n :}
   len CDIGEST:SLOT-BYTES / {: slots:n :}
   at slots + SLOT-CAP > if E-CBIND-PREIMAGE throw then
   slots 0 ?do
      src i CDIGEST:SLOT@  PRE at i + CDIGEST:SLOT!
   loop
   at slots + ;

public

\ ---- construction and validation --------------------------------------------
\ The production entry point. Both halves are revalidated and the pair rule is
\ applied; an unrealisable pairing throws a named error and no binding is made.
: BIND ( CTARGET:contract CNUM:numeric-policy -- CBIND:binding )
   CNUM:VALIDATE CNUM-NUMERIC--POLICY:UNMAKE
   {: o:CNUM:overflow f:CNUM:float-model c:CNUM:contraction
      m:CNUM:fast-math k:CNUM:compare :}
   CTARGET:VALIDATE CTARGET-CONTRACT:UNMAKE
   {: a:CTARGET:arch b:CTARGET:abi e:CTARGET:endian
      p:CTARGET:ptr-width ft:CTARGET:features :}
   c ft FMA-CK
   a b e p ft CTARGET-CONTRACT:MAKE
   o f c m k CNUM-NUMERIC--POLICY:MAKE
   CBIND-BINDING:MAKE ;

\ Recheck a binding that may have been assembled by the generated constructor.
: VALIDATE ( CBIND:binding -- CBIND:binding )
   CBIND-BINDING:UNMAKE BIND ;

\ ---- field readers -----------------------------------------------------------
: TARGET@ ( CBIND:binding -- CTARGET:contract )
   CBIND-BINDING:UNMAKE
   CNUM-NUMERIC--POLICY:UNMAKE drop drop drop drop drop ;

: POLICY@ ( CBIND:binding -- CNUM:numeric-policy )
   CBIND-BINDING:UNMAKE
   CNUM-NUMERIC--POLICY:UNMAKE
   {: o:CNUM:overflow f:CNUM:float-model c:CNUM:contraction
      m:CNUM:fast-math k:CNUM:compare :}
   CTARGET-CONTRACT:UNMAKE drop drop drop drop drop
   o f c m k CNUM-NUMERIC--POLICY:MAKE ;

\ ---- identity ----------------------------------------------------------------
\ Structural identity, delegated to each half's own field-by-field comparison.
: SAME? ( CBIND:binding CBIND:binding -- bool )
   CBIND-BINDING:UNMAKE
   CNUM-NUMERIC--POLICY:UNMAKE
   {: yo:CNUM:overflow yf:CNUM:float-model yc:CNUM:contraction
      ym:CNUM:fast-math yk:CNUM:compare :}
   CTARGET-CONTRACT:UNMAKE
   {: ya:CTARGET:arch yb:CTARGET:abi ye:CTARGET:endian
      yp:CTARGET:ptr-width yft:CTARGET:features :}
   CBIND-BINDING:UNMAKE
   yo yf yc ym yk CNUM-NUMERIC--POLICY:MAKE
   CNUM:SAME? {: same:bool :}
   ya yb ye yp yft CTARGET-CONTRACT:MAKE
   CTARGET:SAME? same and ;

\ The canonical preimage: binding tag, binding schema version, the target
\ contract's whole preimage, the numerical policy's whole preimage. The bytes
\ live in this module and stay valid until the next ENCODE call.
: ENCODE ( CBIND:binding -- ptr u8 n )
   VALIDATE CBIND-BINDING:UNMAKE
   CNUM-NUMERIC--POLICY:UNMAKE
   {: o:CNUM:overflow f:CNUM:float-model c:CNUM:contraction
      m:CNUM:fast-math k:CNUM:compare :}
   CDIGEST:TAG-BINDING PRE SLOT-TAG CDIGEST:SLOT!
   SCHEMA PRE SLOT-SCHEMA CDIGEST:SLOT!
   CTARGET:ENCODE SLOT-HEAD APPEND {: at:n :}
   o f c m k CNUM-NUMERIC--POLICY:MAKE CNUM:ENCODE at APPEND {: end:n :}
   PRE end CDIGEST:SLOT-BYTES * ;

: DIGEST ( CBIND:binding -- CDIGEST:digest )
   ENCODE CDIGEST:COMPUTE ;

;package
