\ schema.f - R7 artifact-indexed evidence families (MODEL-CAD-V2-PLAN.md "R7
\ Design Addendum" § Artifact-indexed evidence families; implementation sub-dot 2
\ v2-evidence-schema-products, dot habu-v2-typestate-evidence-f124dc85).
\
\ One family per evidence CLASS (certified / golden / gradchecked / profiled),
\ each a PRODUCT carrying the exact CAD-KIND:artifact-id it was measured on plus a
\ class-private proof token. The CLASS IS THE FAMILY: there is no (tag, gate-id)
\ raw pair anywhere, so the REPORT:GATE! ( report ptr u8 n n n -- report )
\ verdict/gate-id slot swap (maki/report.f:592, census probe 1) is UNREPRESENTABLE
\ here. A proof token exists ONLY if a transition minted it: every MINT-*-PROOF is
\ PRIVATE, so a raw n cannot forge evidence and a wrong-class token cannot stand in
\ for another class (certify-proof and golden-proof are distinct families).
\
\ Device-golden provenance is a VALUE, not ambient process state: the golden
\ product's `leg` (golden-leg) and `prec` (prec-class) FIELDS are the typed home of
\ what maki/golden.f's GOLDEN-DEV-FLAG / GOLDEN-PREC-V globals used to track
\ (host-vs-device leg, judged precision). Those ambient globals are now RETIRED by the
\ store-seal sub-dot (habu-v2-typestate-promotion-2266b236): the promote path (maki/cad.f)
\ threads these typed EVID:golden-leg / EVID:prec-class values from the golden gate into the
\ (now sealed) evidence writer, so provenance is a value in transit, not process state.
\
\ DEVIATION from the R7 addendum's `result<EVID:class,diag-set>` transition
\ signatures: like the stage transitions (maki/typestate.f), the skeleton evidence
\ transitions cannot fail yet (no gate validation until the promotion-policy
\ sub-dot) and a polymorphic RESULT-DROP over result<a,b> is not expressible
\ (dot habu-typestate-result-drop-5ae048a7). So EVID:CERTIFY/GOLDEN/GRADCHECK/
\ PROFILE return the evidence product DIRECTLY; the error-carrying result arrives
\ when they gain a real diagnostic path in the promotion sub-dots. The artifact
\ binding and class separation - this sub-dot's acceptance - hold either way.
\
\ DEVIATION from the addendum's slot spelling `VARIANT got ... / VARIANT absent`
\ repeated per class: variant names are package-scoped (TDECL-VAR-SCOPE?,
\ src/core/sumtype.f), so four slots in package EVID cannot all reuse `got`/
\ `absent`. Each slot uses class-prefixed variant names (certify-got/-none, ...).
\ The shape - a present-with-typed-evidence arm and an absent arm - is identical.
\
\ SCOPE: the EVID schema and its 4 gate transition words (the R7 addendum assigns
\ EVID:CERTIFY/GOLDEN/GRADCHECK/PROFILE to this sub-dot). The slot sums + bundle
\ are the presence schema consumed by POLICY:CHECK in the promotion-policy sub-dot
\ (habu-v2-typestate-promotion-d539e648); they are declared here as the evidence
\ owner's surface. Identity threading is now LANDED (dot
\ habu-public-producers-for-7084d81c): maki/typestate.f's ART:built is a PRODUCT
\ carrying its CAD-KIND:artifact-id, so each transition READS the artifact from the
\ built witness (ART-BUILT:UNMAKE) rather than taking it as a separately-fabricated
\ operand - the evidence a transition mints is bound to the artifact that was built.

require maki/typestate.f

package EVID
public

\ ---- class-private proof tokens (minted only by the transitions below) -------
TYPEFAMILY certify-proof 0
TYPEFAMILY golden-proof 0
TYPEFAMILY gradcheck-proof 0
TYPEFAMILY profile-proof 0

\ ---- golden provenance, moved out of maki/golden.f ambient globals -----------
\ golden-leg: which leg produced the golden verdict (GOLDEN-DEV-FLAG generalised:
\ host/external self-consistency vs the device model-golden leg).
ENUM golden-leg DERIVE eq
   host
   external
   device
;ENUM
\ prec-class: the precision the golden verdict was judged under (GOLDEN-PREC-V).
\ Class-prefixed variant tails: the bare dtype tails `f32`/`tf32` are reserved
\ atom tokens (src/core/sumtype.f TDECL-RESERVED?).
ENUM prec-class DERIVE eq
   prec-f32
   prec-tf32
;ENUM

\ ---- one PRODUCT per evidence class, each bound to its artifact ---------------
PRODUCT certified 0
   FIELD art CAD-KIND:artifact-id
   FIELD tok certify-proof
;PRODUCT

PRODUCT golden 0
   FIELD art  CAD-KIND:artifact-id
   FIELD leg  golden-leg
   FIELD prec prec-class
   FIELD tok  golden-proof
;PRODUCT

PRODUCT gradchecked 0
   FIELD art CAD-KIND:artifact-id
   FIELD tok gradcheck-proof
;PRODUCT

PRODUCT profiled 0
   FIELD art CAD-KIND:artifact-id
   FIELD us  n
   FIELD tok profile-proof
;PRODUCT

\ ---- per-class presence slots + the promotion bundle -------------------------
\ A slot is `got <evidence>` or absent; the bundle is one slot per class. Consumed
\ by POLICY:CHECK (promotion-policy sub-dot). Class-prefixed variant names because
\ variant names are package-scoped.
SUMTYPE certify-slot 0
   VARIANT certify-got certified ;VARIANT
   VARIANT certify-none ;VARIANT
;SUMTYPE
SUMTYPE golden-slot 0
   VARIANT golden-got golden ;VARIANT
   VARIANT golden-none ;VARIANT
;SUMTYPE
SUMTYPE gradcheck-slot 0
   VARIANT gradcheck-got gradchecked ;VARIANT
   VARIANT gradcheck-none ;VARIANT
;SUMTYPE
SUMTYPE profile-slot 0
   VARIANT profile-got profiled ;VARIANT
   VARIANT profile-none ;VARIANT
;SUMTYPE

PRODUCT bundle 0
   FIELD cert certify-slot
   FIELD gold golden-slot
   FIELD grad gradcheck-slot
   FIELD prof profile-slot
;PRODUCT

private
\ Each mint is the only producer of its class's proof token. A phantom witness:
\ its EXISTENCE (only downstream of a real transition) is the proof, not its value.
TRUSTED: MINT-CERTIFY-PROOF ( -- certify-proof )  0 ;
TRUSTED: MINT-GOLDEN-PROOF ( -- golden-proof )  0 ;
TRUSTED: MINT-GRADCHECK-PROOF ( -- gradcheck-proof )  0 ;
TRUSTED: MINT-PROFILE-PROOF ( -- profile-proof )  0 ;

public
\ ---- gate transition words: read the artifact from the built witness, mint --------
\ Each UNMAKEs ART:built to its (art, build-proof) and drops the build-proof (holding a
\ built already proved the build ran); CERTIFY / GRADCHECK need only the artifact,
\ GOLDEN also carries the leg + precision (the retired ambient globals), PROFILE the
\ measured us. The minted evidence's `art` is the artifact that was actually built.
: CERTIFY ( ART:built -- certified )
   ART-BUILT:UNMAKE drop                  \ built -> art (drop build-proof)
   MINT-CERTIFY-PROOF  EVID-CERTIFIED:MAKE ;
: GOLDEN ( ART:built golden-leg prec-class -- golden )
   {: leg:golden-leg prec:prec-class :}   \ pop leg + prec; the built witness remains
   ART-BUILT:UNMAKE drop                  \ built -> art
   leg prec  MINT-GOLDEN-PROOF  EVID-GOLDEN:MAKE ;
: GRADCHECK ( ART:built -- gradchecked )
   ART-BUILT:UNMAKE drop
   MINT-GRADCHECK-PROOF  EVID-GRADCHECKED:MAKE ;
: PROFILE ( ART:built n -- profiled )
   {: us:n :}                             \ pop measured us; the built witness remains
   ART-BUILT:UNMAKE drop
   us  MINT-PROFILE-PROOF  EVID-PROFILED:MAKE ;
;package
