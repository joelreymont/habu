\ schema-test.f - R7 artifact-indexed evidence acceptance suite (sub-dot
\ v2-evidence-schema-products, dot habu-v2-typestate-evidence-f124dc85).
\
\ Pins the addendum's evidence layer (MODEL-CAD-V2-PLAN.md § Artifact-indexed
\ evidence families, fixtures at :1840-1882) against the landed schema. Every
\ negative is paired with a resolving positive control (per LESSONS).
\ CHECK-QUIET-CANDIDATE! returns -1 certified / 0 type-reject / 1 unresolvable
\ (sealed private word not visible); both 0 and 1 are "fails to certify"
\ (test/checker-assert.f).
\
\ How this closes the V1 census (MODEL-CAD-V2-PLAN.md:1564-1583):
\ - probe 1 (verdict/gate-id slot mixing in REPORT:GATE!, :1564-1570): there is no
\   (tag, gate-id) raw pair; the CLASS is the FAMILY. EV-BAD-CLASS and
\   EV-BAD-BUNDLE show a wrong-class token/slot is a type mismatch, not a
\   swappable int.
\ - probe 3 (wrong-artifact evidence / store bypass, :1576-1583 "Nothing ties a
\   verdict to the artifact it was measured on"): every class STRUCTURE binds a
\   CAD-KIND:artifact-id and a class-private proof token. TS-BAD-EVID-RAW (raw n
\   for the token), TS-BAD-EVID-ID (foreign id family for the artifact), and
\   TS-BAD-EVID-MINT (the proof mint is private, unresolvable outside EVID) show
\   evidence cannot be forged or mis-bound.
\ The executed-value artifact-mismatch negative (a bundle for artifact B refused
\ for artifact A with E-EVID-ARTIFACT, plan:1866-1869) needs POLICY:CHECK and is
\ owned by the promotion-policy sub-dot; it is out of this sub-dot's scope.
\ The retired raw REPORT:GATE! (report, TS-OLD-GATE) is demoted by the
\ report-render sub-dot; it stays public here, so it is not asserted retired.

require lib/test.f
require test/checker-assert.f      \ CHECK-QUIET-CANDIDATE! plus REFLECT, which reads the registry
require maki/evidence/schema.f

T-RESET

\ ---- positive controls: born-typed construction of each class -----------------
s" EV-OK-CERT ( CAD-KIND:artifact-id EVID:certify-proof -- EVID:certified ) EVID-CERTIFIED:MAKE"
   CHECK-QUIET-CANDIDATE! -1 T=
s" EV-OK-GOLD ( CAD-KIND:artifact-id EVID:golden-leg EVID:prec-class NPOL:dom EVID:golden-proof -- EVID:golden ) EVID-GOLDEN:MAKE"
   CHECK-QUIET-CANDIDATE! -1 T=
\ GOLD-DOM projects the golden's achieved numeric proof domain (the promotion gate reads it).
s" EV-OK-GOLD-DOM ( EVID:golden -- NPOL:dom ) EVID:GOLD-DOM"
   CHECK-QUIET-CANDIDATE! -1 T=
\ a wrong-family value in the numeric-policy slot (prec-class where NPOL:dom is expected) rejects.
s" EV-BAD-GOLD-POL ( CAD-KIND:artifact-id EVID:golden-leg EVID:prec-class EVID:prec-class EVID:golden-proof -- EVID:golden ) EVID-GOLDEN:MAKE"
   CHECK-QUIET-CANDIDATE! 0 T=
s" EV-OK-GRAD ( CAD-KIND:artifact-id EVID:gradcheck-proof -- EVID:gradchecked ) EVID-GRADCHECKED:MAKE"
   CHECK-QUIET-CANDIDATE! -1 T=
s" EV-OK-PROF ( CAD-KIND:artifact-id n EVID:profile-proof -- EVID:profiled ) EVID-PROFILED:MAKE"
   CHECK-QUIET-CANDIDATE! -1 T=

\ ---- positive controls: the gate transitions type-check in the right order ----
\ (ART:built is minted only inside package ART, so, like CAND:EMIT/ART:BUILD in
\ maki/typestate-test.f, the transitions are pinned by type candidates, not executed;
\ maki/evidence/policy-e2e-test.f runs them over a real built witness.) The artifact
\ id is READ from ART:built now, so the transitions no longer take it as an operand.
s" EV-OK-CERTIFY ( ART:built -- EVID:certified ) EVID:CERTIFY"
   CHECK-QUIET-CANDIDATE! -1 T=
s" EV-OK-GOLDEN ( ART:built EVID:golden-leg EVID:prec-class NPOL:dom -- EVID:golden ) EVID:GOLDEN"
   CHECK-QUIET-CANDIDATE! -1 T=
s" EV-OK-GRADCHECK ( ART:built -- EVID:gradchecked ) EVID:GRADCHECK"
   CHECK-QUIET-CANDIDATE! -1 T=
s" EV-OK-PROFILE ( ART:built n -- EVID:profiled ) EVID:PROFILE"
   CHECK-QUIET-CANDIDATE! -1 T=

\ ---- wrong-artifact-evidence negatives (plan:1856-1865) -----------------------
\ raw cell where the class proof token is expected -> type reject (verdict 0).
s" TS-BAD-EVID-RAW ( CAD-KIND:artifact-id n -- EVID:certified ) EVID-CERTIFIED:MAKE"
   CHECK-QUIET-CANDIDATE! 0 T=
\ a foreign same-width id family where the artifact id is expected -> reject.
s" TS-BAD-EVID-ID ( CAD-KIND:evidence-id EVID:certify-proof -- EVID:certified ) EVID-CERTIFIED:MAKE"
   CHECK-QUIET-CANDIDATE! 0 T=
\ the proof mint is private to EVID: unresolvable outside its owner (verdict 1).
s" TS-BAD-EVID-MINT ( CAD-KIND:artifact-id -- EVID:certified ) MINT-CERTIFY-PROOF EVID-CERTIFIED:MAKE"
   CHECK-QUIET-CANDIDATE! 1 T=

\ ---- wrong-class negatives: the class IS the family ---------------------------
\ a golden proof token cannot fill a certify structure's token slot.
s" EV-BAD-CLASS-TOK ( CAD-KIND:artifact-id EVID:golden-proof -- EVID:certified ) EVID-CERTIFIED:MAKE"
   CHECK-QUIET-CANDIDATE! 0 T=
\ a certified value cannot stand in for a golden value.
s" EV-BAD-CLASS-VAL ( EVID:certified -- EVID:golden ) "
   CHECK-QUIET-CANDIDATE! 0 T=
\ feeding a gradchecked artifact to EVID:CERTIFY (expects ART:built) rejects.
s" EV-BAD-CERTIFY-IN ( EVID:gradchecked -- EVID:certified ) EVID:CERTIFY"
   CHECK-QUIET-CANDIDATE! 0 T=

\ ---- presence bundle: schema is well-formed; slot order is type-checked -------
s" EV-OK-BUNDLE ( EVID:certify-slot EVID:golden-slot EVID:gradcheck-slot EVID:profile-slot -- EVID:bundle ) EVID-BUNDLE:MAKE"
   CHECK-QUIET-CANDIDATE! -1 T=
\ swapping the certify and golden slots is a type mismatch (no raw tag to mix).
s" EV-BAD-BUNDLE ( EVID:golden-slot EVID:certify-slot EVID:gradcheck-slot EVID:profile-slot -- EVID:bundle ) EVID-BUNDLE:MAKE"
   CHECK-QUIET-CANDIDATE! 0 T=

\ ---- the four presence slots as full-mode ENUMs --------------------------------
\ The slots are declared through the unified ENUM front end, so each `got` arm's
\ payload is a named FIELD. These pins are the migration's identity proof: the
\ generated constructor spelling, the exact checked effect, and the field NAME to
\ payload SLOT mapping the declaration published. REFLECT (test/checker-assert.f)
\ reads the registry; this package holds only the identities this suite pins - a
\ family tail plus the constructor package its variants carry, exactly the
\ (package, tail) pair that owns family identity, so a pin names the family it pins
\ instead of guessing from shape. Every slot family declares `got` as arm 0 and
\ `none` as arm 1, so the pins below carry that arm index explicitly.
package EVID-SLOT
public

\ the four (tail, constructor package) identities this file pins
: CERT$ ( -- ptr u8 n ptr u8 n )   s" certify-slot" s" EVID-CERTIFY--SLOT" ;
: GOLD$ ( -- ptr u8 n ptr u8 n )   s" golden-slot" s" EVID-GOLDEN--SLOT" ;
: GRAD$ ( -- ptr u8 n ptr u8 n )   s" gradcheck-slot" s" EVID-GRADCHECK--SLOT" ;
: PROF$ ( -- ptr u8 n ptr u8 n )   s" profile-slot" s" EVID-PROFILE--SLOT" ;

;package

\ generated constructor spelling + exact effect, per arm. A YES demands verdict -1
\ and the checker answers 1 for a name it cannot resolve, so -1 proves it resolved
\ EXACTLY this constructor name; the NO cases resolved the name and refused the types.
s" EV-CG ( EVID:certified -- EVID:certify-slot ) EVID-CERTIFY--SLOT:CERTIFY-GOT"
   CHECK-QUIET-CANDIDATE! -1 T=
s" EV-CN ( -- EVID:certify-slot ) EVID-CERTIFY--SLOT:CERTIFY-NONE"
   CHECK-QUIET-CANDIDATE! -1 T=
s" EV-GG ( EVID:golden -- EVID:golden-slot ) EVID-GOLDEN--SLOT:GOLDEN-GOT"
   CHECK-QUIET-CANDIDATE! -1 T=
s" EV-GN ( -- EVID:golden-slot ) EVID-GOLDEN--SLOT:GOLDEN-NONE"
   CHECK-QUIET-CANDIDATE! -1 T=
s" EV-DG ( EVID:gradchecked -- EVID:gradcheck-slot ) EVID-GRADCHECK--SLOT:GRADCHECK-GOT"
   CHECK-QUIET-CANDIDATE! -1 T=
s" EV-DN ( -- EVID:gradcheck-slot ) EVID-GRADCHECK--SLOT:GRADCHECK-NONE"
   CHECK-QUIET-CANDIDATE! -1 T=
s" EV-PG ( EVID:profiled -- EVID:profile-slot ) EVID-PROFILE--SLOT:PROFILE-GOT"
   CHECK-QUIET-CANDIDATE! -1 T=
s" EV-PN ( -- EVID:profile-slot ) EVID-PROFILE--SLOT:PROFILE-NONE"
   CHECK-QUIET-CANDIDATE! -1 T=

\ forge negatives on the payload: a raw cell cannot fill it, the payload is
\ mandatory on the got arm and forbidden on the none arm, the slot is not a bare
\ scalar, and the payload is CLASS-typed - another class's evidence is refused even
\ though both are records of the same width (certified and gradchecked are both 2).
s" EV-CG-RAW ( n -- EVID:certify-slot ) EVID-CERTIFY--SLOT:CERTIFY-GOT"
   CHECK-QUIET-CANDIDATE! 0 T=
s" EV-CG-NONE ( -- EVID:certify-slot ) EVID-CERTIFY--SLOT:CERTIFY-GOT"
   CHECK-QUIET-CANDIDATE! 0 T=
s" EV-CN-PAY ( EVID:certified -- EVID:certify-slot ) EVID-CERTIFY--SLOT:CERTIFY-NONE"
   CHECK-QUIET-CANDIDATE! 0 T=
s" EV-CG-BARE ( EVID:certified -- n ) EVID-CERTIFY--SLOT:CERTIFY-GOT"
   CHECK-QUIET-CANDIDATE! 0 T=
s" EV-CG-XCLASS ( EVID:gradchecked -- EVID:certify-slot ) EVID-CERTIFY--SLOT:CERTIFY-GOT"
   CHECK-QUIET-CANDIDATE! 0 T=
s" EV-DG-XCLASS ( EVID:certified -- EVID:gradcheck-slot ) EVID-GRADCHECK--SLOT:GRADCHECK-GOT"
   CHECK-QUIET-CANDIDATE! 0 T=
\ a slot never stands in for another slot family, in either direction.
s" EV-CG-XSLOT ( EVID:certified -- EVID:gradcheck-slot ) EVID-CERTIFY--SLOT:CERTIFY-GOT"
   CHECK-QUIET-CANDIDATE! 0 T=
s" EV-SLOT-XVAL ( EVID:certify-slot -- EVID:gradcheck-slot ) "
   CHECK-QUIET-CANDIDATE! 0 T=

\ the published shape of each slot family: exactly one family answers to each
\ (tail, constructor package), the arms keep their names and order, the width is
\ unchanged by the migration, and the got arm carries its evidence in ONE named
\ cell `ev` at payload slot 0 while the none arm carries no named cell at all.
EVID-SLOT:CERT$ REFLECT:FAMS 1 T=
EVID-SLOT:GOLD$ REFLECT:FAMS 1 T=
EVID-SLOT:GRAD$ REFLECT:FAMS 1 T=
EVID-SLOT:PROF$ REFLECT:FAMS 1 T=
EVID-SLOT:CERT$ REFLECT:VARS 2 T=
EVID-SLOT:GOLD$ REFLECT:VARS 2 T=
EVID-SLOT:GRAD$ REFLECT:VARS 2 T=
EVID-SLOT:PROF$ REFLECT:VARS 2 T=
EVID-SLOT:CERT$ 0 REFLECT:ARM$ s" certify-got" T$=
EVID-SLOT:CERT$ 1 REFLECT:ARM$ s" certify-none" T$=
EVID-SLOT:GOLD$ 0 REFLECT:ARM$ s" golden-got" T$=
EVID-SLOT:GOLD$ 1 REFLECT:ARM$ s" golden-none" T$=
EVID-SLOT:GRAD$ 0 REFLECT:ARM$ s" gradcheck-got" T$=
EVID-SLOT:GRAD$ 1 REFLECT:ARM$ s" gradcheck-none" T$=
EVID-SLOT:PROF$ 0 REFLECT:ARM$ s" profile-got" T$=
EVID-SLOT:PROF$ 1 REFLECT:ARM$ s" profile-none" T$=
EVID-SLOT:CERT$ REFLECT:WIDTH 3 T=       \ a 2-cell certified plus one tag cell
EVID-SLOT:GOLD$ REFLECT:WIDTH 6 T=       \ a 5-cell golden plus one tag cell
EVID-SLOT:GRAD$ REFLECT:WIDTH 3 T=
EVID-SLOT:PROF$ REFLECT:WIDTH 4 T=       \ a 3-cell profiled plus one tag cell
EVID-SLOT:CERT$ 0 REFLECT:ARM-FLDS 1 T=
EVID-SLOT:GOLD$ 0 REFLECT:ARM-FLDS 1 T=
EVID-SLOT:GRAD$ 0 REFLECT:ARM-FLDS 1 T=
EVID-SLOT:PROF$ 0 REFLECT:ARM-FLDS 1 T=
EVID-SLOT:CERT$ 1 REFLECT:ARM-FLDS 0 T=
EVID-SLOT:GOLD$ 1 REFLECT:ARM-FLDS 0 T=
EVID-SLOT:GRAD$ 1 REFLECT:ARM-FLDS 0 T=
EVID-SLOT:PROF$ 1 REFLECT:ARM-FLDS 0 T=
EVID-SLOT:CERT$ 0 s" ev" REFLECT:ARM-SLOT 0 T=
EVID-SLOT:GOLD$ 0 s" ev" REFLECT:ARM-SLOT 0 T=
EVID-SLOT:GRAD$ 0 s" ev" REFLECT:ARM-SLOT 0 T=
EVID-SLOT:PROF$ 0 s" ev" REFLECT:ARM-SLOT 0 T=
\ the payload cell width is the carried class's own width, so a slot that named the
\ wrong class would report the wrong number here even at equal tag position.
EVID-SLOT:CERT$ 0 s" ev" REFLECT:ARM-CELLS 2 T=
EVID-SLOT:GOLD$ 0 s" ev" REFLECT:ARM-CELLS 5 T=
EVID-SLOT:GRAD$ 0 s" ev" REFLECT:ARM-CELLS 2 T=
EVID-SLOT:PROF$ 0 s" ev" REFLECT:ARM-CELLS 3 T=
\ an undeclared name resolves to no slot on the got arm.
EVID-SLOT:CERT$ 0 s" evid" REFLECT:ARM-SLOT -1 T=
EVID-SLOT:GOLD$ 0 s" art" REFLECT:ARM-SLOT -1 T=

T-REPORT
