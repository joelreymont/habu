\ policy.f - R7 promotion-policy products (MODEL-CAD-V2-PLAN.md "R7 Design
\ Addendum" § Promotion-policy products; implementation sub-dot 3
\ v2-promotion-policy, dot habu-v2-typestate-promotion-d539e648).
\
\ Policies are ordinary public DATA (inputs, not proofs): a `gate-set` is one
\ `req` (requirement class) per evidence class plus the policy's schema identity.
\ The GRANT is sealed: a `granted` exists ONLY if POLICY:CHECK minted it after the
\ value-level artifact binding held. MINT-GRANT-PROOF is PRIVATE, so a raw n cannot
\ forge a grant and no code outside POLICY can produce POLICY:granted even though
\ POLICY-GRANTED:MAKE resolves (plan §24 Proof Theater: the type system guarantees
\ provenance (evidence proof tokens only exist downstream of a real gate) and stage
\ order; POLICY:CHECK is the ONE site where same-family artifact-id equality is
\ checked, inside the only word that can mint POLICY:granted).
\
\ POLICY:CHECK consumes the EVID:bundle / slot schema from maki/evidence/schema.f:
\ for each slot the policy marks required, the evidence value must be `got` and its
\ `art` field must equal the artifact under promotion (E-EVID-ARTIFACT on mismatch;
\ E-EVID-MISSING when a required slot is absent).
\
\ DEVIATION 1 (refusal channel): the R7 addendum types the transitions
\ `result<_,diag-set>`, and the schema.f / typestate.f skeletons deferred the
\ error-carrying result to this promotion sub-dot. But `result<POLICY:granted,_>`
\ is NOT expressible: a result payload param is single-cell, and `granted` is a
\ multi-cell product (art+pol+tok) - the checker rejects `RESULT:OK` on it
\ ("expected: a actual: granted<>"), the same multi-cell-payload wall the schema.f
\ RESULT-DROP deviation names (dot habu-typestate-result-drop-5ae048a7). So CHECK
\ returns POLICY:granted DIRECTLY and signals a refusal by THROWing a named code
\ (E-EVID-ARTIFACT / E-EVID-MISSING), exactly as V1 PROMOTE-REPORT throws E-CAD-GATE
\ (maki/cad.f:1036). The refusal path is real and named; a result<granted,diag-set>
\ must wait for a multi-cell result-payload capability (that dot).
\
\ DEVIATION 2 (explicit artifact operand): the addendum spells POLICY:CHECK
\ ( ART:built EVID:bundle POLICY:promote-policy -- ... ), but ART:built is a
\ fieldless stage nominal (maki/typestate.f) that carries no artifact id, so the
\ artifact under promotion is passed as an explicit CAD-KIND:artifact-id operand -
\ exactly as schema.f's EVID transitions take the artifact id explicitly because
\ identity threading through the fieldless stages is not built yet. ART:built is
\ kept as the type-level "was actually built" witness (so a permissive policy over
\ an empty bundle cannot grant for a never-built artifact).
\
\ DEVIATION 3 (family names): the addendum names the families `req-class` and
\ `promote-policy`. Their generated constructor-package names under package POLICY
\ escape to `POLICY-REQ--CLASS` (17) and `POLICY-PROMOTE--POLICY` (22), both over
\ TF-CTOR-NAME-LIMIT=16 (src/core/type-family.f:604), so they fall back to opaque
\ SHA-named ctor packages (`Tefd3bc6...-REQ-CLASS`) that cannot construct the value
\ in readable committed source - and DEFAULT-POLICY / POLICY:CHECK must construct
\ them. They are renamed to `req` (POLICY-REQ) and `gate-set` (POLICY-GATE--SET),
\ which keep clean ctor packages and the meaning (a per-class requirement; the
\ promotion gate set, matching the plan's "V1 gate set = default policy"). The
\ same >16 overflow makes EVID's slot sums (EVID-CERTIFY--SLOT, ...) constructible
\ only via SHA names, so a full EVID:bundle cannot be built in readable test source;
\ see maki/evidence/policy-test.f for how the value-level binding is proven instead.
\ Both want a checker dot to raise TF-CTOR-NAME-LIMIT / add short ctor aliases.

require lib/prelude.f
require maki/evidence/schema.f

\ maki owns -5000..-5099; evidence-policy uses -5098..-5099 (the E-RPT-GATE
\ report.f:34 owning-file convention).
-5098 constant E-EVID-ARTIFACT   \ a required evidence slot names a different artifact than the one under promotion
-5099 constant E-EVID-MISSING    \ a required evidence slot is absent

package POLICY
public

\ grant-proof: the sealed promotion witness; only MINT-GRANT-PROOF produces one.
TYPEFAMILY grant-proof 0

\ req: how strongly a policy requires each evidence class. Ordinary policy DATA,
\ so DERIVE eq gives POLICY-REQ:EQ ( req req -- bool ) for schema comparison.
\   required-blocking       - evidence must exist and carry this artifact
\   required-when-supported - if present must bind; absent is an accepted unsupported case
\   required-recorded       - must exist and bind; content never blocks (profile)
\   informational           - may be absent; never checked
ENUM req DERIVE eq
   required-blocking
   required-when-supported
   required-recorded
   informational
;ENUM

\ gate-set: a promotion policy - one req per evidence class + the policy's schema
\ identity (`pol` participates in the release key, plan §16).
PRODUCT gate-set 0
   FIELD cert req
   FIELD gold req
   FIELD grad req
   FIELD prof req
   FIELD pol  CAD-KIND:schema-id
;PRODUCT

\ granted: the sealed grant - bound to its artifact, its policy identity, and a
\ private proof token that only POLICY:CHECK can mint.
PRODUCT granted 0
   FIELD art CAD-KIND:artifact-id
   FIELD pol CAD-KIND:schema-id
   FIELD tok grant-proof
;PRODUCT

private

\ MINT-GRANT-PROOF: the ONLY producer of a grant-proof cell, invoked by CHECK
\ after the value-level binding held (the maki/target/target.f:54 mint pattern).
TRUSTED: MINT-GRANT-PROOF ( -- grant-proof )  0 ;

\ AID>RAW: the audited value-level projection of an artifact-id identity to a raw
\ cell, used ONLY to compare two artifact-ids (the checker has no artifact-id
\ equality). The maki/target/target.f:55 TARGET-ID>RAW precedent; family-in /
\ raw-out, so it is a projection boundary, not a forging mint.
TRUSTED: AID>RAW ( CAD-KIND:artifact-id -- n )  ;

\ AID=: checked artifact-id identity equality built over the projection boundary.
: AID= ( CAD-KIND:artifact-id CAD-KIND:artifact-id -- bool )
   AID>RAW swap AID>RAW = ;

\ ---- per-class requirement decisions (MATCH on the policy's req value) --------
: REQ-MUST-EXIST? ( req -- bool )         \ blocking/recorded require the slot present
   MATCH req
      required-blocking       OF true  ENDOF
      required-when-supported OF false ENDOF
      required-recorded       OF true  ENDOF
      informational           OF false ENDOF
   ;MATCH ;
: REQ-BINDS-IF-PRESENT? ( req -- bool )   \ every non-informational class binds a present value
   MATCH req
      required-blocking       OF true  ENDOF
      required-when-supported OF true  ENDOF
      required-recorded       OF true  ENDOF
      informational           OF false ENDOF
   ;MATCH ;

\ SLOT-ERR: the refusal code (0 = ok) for one slot given whether its evidence is
\ present (bool 1), whether - if present - it binds the artifact under promotion
\ (bool 2), and the class requirement. This is the whole promotion decision table.
: SLOT-ERR ( bool bool req -- n )
   {: p:bool m:bool r:req :}
   p if
      r REQ-BINDS-IF-PRESENT? if
         m if 0 else E-EVID-ARTIFACT then
      else 0 then
   else
      r REQ-MUST-EXIST? if E-EVID-MISSING else 0 then
   then ;

: FIRST-ERR ( n n -- n )                  \ keep the first nonzero refusal code
   over 0= if nip else drop then ;

\ ---- artifact projections of each evidence class (drop the class proof token) --
: CERT-ART ( EVID:certified -- CAD-KIND:artifact-id )     EVID-CERTIFIED:UNMAKE drop ;
: GOLD-ART ( EVID:golden -- CAD-KIND:artifact-id )        EVID-GOLDEN:UNMAKE drop drop drop ;
: GRAD-ART ( EVID:gradchecked -- CAD-KIND:artifact-id )   EVID-GRADCHECKED:UNMAKE drop ;
: PROF-ART ( EVID:profiled -- CAD-KIND:artifact-id )      EVID-PROFILED:UNMAKE drop drop ;

\ ---- one verdict per slot: MATCH the presence sum, bind the artifact, decide ---
\ A tagged-sum slot cannot be a typed local (only single-cell nominals/enums can),
\ so the slot stays DEEPEST and the artifact id + req (single-cell) pop as locals,
\ leaving the slot on top for MATCH; the got arm reads the artifact-under-promotion
\ local `a` to bind the evidence's `art` field.
: CERT-VERDICT ( EVID:certify-slot CAD-KIND:artifact-id req -- n )
   {: a:CAD-KIND:artifact-id r:req :}
   MATCH certify-slot
      certify-got  OF CERT-ART a AID= true swap ENDOF
      certify-none OF false false ENDOF
   ;MATCH  r SLOT-ERR ;
: GOLD-VERDICT ( EVID:golden-slot CAD-KIND:artifact-id req -- n )
   {: a:CAD-KIND:artifact-id r:req :}
   MATCH golden-slot
      golden-got  OF GOLD-ART a AID= true swap ENDOF
      golden-none OF false false ENDOF
   ;MATCH  r SLOT-ERR ;
: GRAD-VERDICT ( EVID:gradcheck-slot CAD-KIND:artifact-id req -- n )
   {: a:CAD-KIND:artifact-id r:req :}
   MATCH gradcheck-slot
      gradcheck-got  OF GRAD-ART a AID= true swap ENDOF
      gradcheck-none OF false false ENDOF
   ;MATCH  r SLOT-ERR ;
: PROF-VERDICT ( EVID:profile-slot CAD-KIND:artifact-id req -- n )
   {: a:CAD-KIND:artifact-id r:req :}
   MATCH profile-slot
      profile-got  OF PROF-ART a AID= true swap ENDOF
      profile-none OF false false ENDOF
   ;MATCH  r SLOT-ERR ;

public

\ DEFAULT-POLICY: EXACTLY the V1 inference gate set (maki/cad.f:1019-1030) -
\ certify + golden block on pass, gradcheck blocks only a real fail
\ (required-when-supported: absent/unsupported clears like V1 not-run), profile is
\ mandatory-to-run but non-blocking (required-recorded). `pol` is the caller's
\ policy schema identity.
: DEFAULT-POLICY ( CAD-KIND:schema-id -- gate-set )
   {: pol:CAD-KIND:schema-id :}
   POLICY-REQ:REQUIRED-BLOCKING
   POLICY-REQ:REQUIRED-BLOCKING
   POLICY-REQ:REQUIRED-WHEN-SUPPORTED
   POLICY-REQ:REQUIRED-RECORDED
   pol
   POLICY-GATE--SET:MAKE ;

\ CHECK: the single sealed site of value-level artifact binding. On a clean policy
\ pass it mints and returns the grant; on refusal it THROWs the first refusal code
\ (cert > gold > grad > prof priority), exactly as V1 PROMOTE-REPORT throws E-CAD-GATE
\ (maki/cad.f:1036). The bundle is DEEPEST so it is UNMAKEd only after the single-cell
\ operands become locals; the four verdict codes accumulate through the return stack
\ because a tagged-sum slot cannot rest under a partial result. ART:built is the
\ type-level "was actually built" witness (consumed, not read).
: CHECK ( EVID:bundle ART:built CAD-KIND:artifact-id gate-set -- POLICY:granted )
   POLICY-GATE--SET:UNMAKE
      {: cReq:req gReq:req grReq:req pReq:req sid:CAD-KIND:schema-id :}
   {: a:CAD-KIND:artifact-id :}      \ pop artifact-id, leaving  bundle built
   drop                             \ drop the ART:built witness, leaving  bundle
   EVID-BUNDLE:UNMAKE               \ stack: certify-slot golden-slot gradcheck-slot profile-slot
   a pReq  PROF-VERDICT >r
   a grReq GRAD-VERDICT >r
   a gReq  GOLD-VERDICT >r
   a cReq  CERT-VERDICT
   r> FIRST-ERR r> FIRST-ERR r> FIRST-ERR
   dup 0= if
      drop  a sid MINT-GRANT-PROOF POLICY-GRANTED:MAKE
   else
      throw
   then ;
;package
