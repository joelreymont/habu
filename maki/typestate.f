\ typestate.f - R7 stage typestate skeleton (MODEL-CAD-V2-PLAN.md "R7 Design
\ Addendum", implementation sub-dot 1 v2-typestate-stage-kinds; dot
\ habu-v2-typestate-stage-a0eb43a2).
\
\ Each Model-CAD IR level is a package (MODEL / TIR / RIR / PLAN / KIR / CAND /
\ ART) exposing sealed arity-0 stage nominals. A stage value exists ONLY if a
\ transition minted it: every RAW>* mint is PRIVATE, so a raw n cannot forge a
\ stage and no caller can fabricate a "verified" object. The transition words
\ thread the pipeline in exactly one legal order, so feeding a wrong-stage value
\ to a transition is a signature mismatch the checker rejects BEFORE runtime -
\ replacing the maki/cad.f runtime order guards (NEED-MODEL / NEED-CAPTURE / each
\ command re-running earlier phases) with a type.
\
\ DEVIATION from the R7 addendum's `result<stage,diag-set>` transition
\ signatures: the skeleton transitions cannot fail (no validation yet - that is
\ sub-dots 2-4's evidence/policy work), and a polymorphic `RESULT-DROP` over
\ `result<a,b>` is not expressible (MATCH on a sum with unknown-width payloads
\ needs a runtime family lookup). So the transitions return the next stage
\ DIRECTLY; the error-carrying `result<_,diag-set>` arrives when the transitions
\ gain a real diagnostic path in the evidence/promotion sub-dots. The stage
\ ORDERING - the acceptance of this sub-dot - is fully enforced either way.
\
\ Bodies are minimal skeletons: consume the input stage(s), mint the output.
\ Identity/provenance threading (carrying the artifact id through the stages) is
\ a refinement for the evidence sub-dots; here each mint takes a fresh 0.
\
\ SCOPE: 7 of the R7 addendum's 13 transition words live here - the STAGE
\ transitions (MODEL:ELABORATE TIR:SOLVE RIR:LEGALIZE PLAN:FINISH KIR:VERIFY
\ CAND:EMIT ART:BUILD). The remaining 6 are owned by the follow-on sub-dots
\ whose families they mint: EVID:CERTIFY/GOLDEN/GRADCHECK/PROFILE by the
\ evidence-schema sub-dot (habu-v2-typestate-evidence-f124dc85), POLICY:CHECK
\ by the promotion-policy sub-dot (habu-v2-typestate-promotion-d539e648), and
\ ART:PROMOTE by the transition/store-seal sub-dot
\ (habu-v2-typestate-promotion-2266b236).

require maki/cad-kinds.f

\ ---- MODEL: declared / elaborated -------------------------------------------
package MODEL
public
TYPEFAMILY decl 0
TYPEFAMILY elab 0
private
TRUSTED: RAW>DECL ( n -- decl ) ;
TRUSTED: RAW>ELAB ( n -- elab ) ;
public
\ DECLARE seeds the pipeline: the only public entry to a MODEL:decl.
: DECLARE ( -- decl )  0 RAW>DECL ;
: ELABORATE ( decl -- elab )  drop  0 RAW>ELAB ;
;package

\ ---- TIR: type/shape solved -------------------------------------------------
package TIR
public
TYPEFAMILY solved 0
private
TRUSTED: RAW>SOLVED ( n -- solved ) ;
public
: SOLVE ( MODEL:elab -- solved )  drop  0 RAW>SOLVED ;
;package

\ ---- RIR: region-legalized --------------------------------------------------
package RIR
public
TYPEFAMILY legal 0
private
TRUSTED: RAW>LEGAL ( n -- legal ) ;
public
: LEGALIZE ( TIR:solved -- legal )  drop  0 RAW>LEGAL ;
;package

\ ---- PLAN: draft / complete -------------------------------------------------
package PLAN
public
TYPEFAMILY draft 0
TYPEFAMILY complete 0
private
TRUSTED: RAW>DRAFT ( n -- draft ) ;
TRUSTED: RAW>COMPLETE ( n -- complete ) ;
public
\ DRAFT seeds an incomplete plan; FINISH promotes it against a legal region ir.
: DRAFT ( -- draft )  0 RAW>DRAFT ;
: FINISH ( RIR:legal draft -- complete )  drop drop  0 RAW>COMPLETE ;
;package

\ ---- KIR: drafted / verified ------------------------------------------------
package KIR
public
TYPEFAMILY drafted 0
TYPEFAMILY verified 0
private
TRUSTED: RAW>DRAFTED ( n -- drafted ) ;
TRUSTED: RAW>VERIFIED ( n -- verified ) ;
public
: DRAFT ( -- drafted )  0 RAW>DRAFTED ;
: VERIFY ( PLAN:complete drafted -- verified )  drop drop  0 RAW>VERIFIED ;
;package

\ ---- CAND: emitted (needs a verified KIR + a target) ------------------------
package CAND
public
TYPEFAMILY emitted 0
private
TRUSTED: RAW>EMITTED ( n -- emitted ) ;
public
: EMIT ( KIR:verified CAD-KIND:target-id -- emitted )  drop drop  0 RAW>EMITTED ;
;package

\ ---- ART: built (needs an emitted candidate + the artifact identity) ---------
\ Identity threading (dot habu-public-producers-for-7084d81c, discharging the
\ refinement the stage/evidence sub-dots deferred): ART:built is now a PRODUCT that
\ CARRIES the CAD-KIND:artifact-id it was built from, so the evidence/policy/promote
\ transitions read the artifact FROM the built witness (maki/evidence/schema.f,
\ maki/evidence/policy.f) instead of taking it as a loose, separately-fabricated
\ operand. A class-private `build-proof` token keeps `built` UNFORGEABLE exactly like
\ the evidence proof tokens (maki/evidence/schema.f): MINT-BUILD-PROOF is PRIVATE, so a
\ caller holding an artifact-id cannot MAKE the "was actually built" witness around
\ ART:BUILD, and therefore cannot forge downstream evidence. The old fieldless-nominal
\ RAW>BUILT mint is retired - a product needs no raw mint.
\
\ The build's artifact id comes from the public producer maki/artifact.f
\ (ARTIFACT:REGISTER). The skeleton's decorative CAD-KIND:toolchain-id operand (it was
\ consumed and dropped, no producer existed) is replaced by that meaningful id; typed
\ toolchain provenance in ART:built is a follow-on if a consumer needs it.
package ART
public
TYPEFAMILY build-proof 0
PRODUCT built 0
   FIELD art CAD-KIND:artifact-id
   FIELD tok build-proof
;PRODUCT
private
TRUSTED: MINT-BUILD-PROOF ( -- build-proof )  0 ;
public
: BUILD ( CAND:emitted CAD-KIND:artifact-id -- built )
   {: art:CAD-KIND:artifact-id :}   \ pop the artifact id; the emitted candidate remains
   drop                              \ consume CAND:emitted (the build ran)
   art MINT-BUILD-PROOF ART-BUILT:MAKE ;
;package
