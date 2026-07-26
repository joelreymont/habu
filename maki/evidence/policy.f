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
\ (was DEVIATION 2, now DISCHARGED - dot habu-public-producers-for-7084d81c):
\ POLICY:CHECK reads the artifact under promotion FROM the ART:built witness. That
\ witness is now a PRODUCT carrying its CAD-KIND:artifact-id (maki/typestate.f), so the
\ artifact is no longer a separately-fabricated operand: CHECK UNMAKEs built for the
\ artifact and binds each present evidence's `art` to it. ART:built stays the
\ unforgeable "was actually built" witness (its build-proof token is minted only by
\ ART:BUILD), so a permissive policy over an empty bundle still cannot grant for a
\ never-built artifact.
\
\ DEVIATION 3 (family names): the addendum names the families `req-class` and
\ `promote-policy`. Their generated constructor-package names under package POLICY
\ escape to `POLICY-REQ--CLASS` (17) and `POLICY-PROMOTE--POLICY` (22), both over
\ TF-CTOR-NAME-LIMIT=16 (src/core/type-family.f:604), so they fall back to opaque
\ SHA-named ctor packages (`Tefd3bc6...-REQ-CLASS`) that cannot construct the value
\ in readable committed source - and DEFAULT-POLICY / POLICY:CHECK must construct
\ them. They are renamed to `req` (POLICY-REQ) and `gate-set` (POLICY-GATE--SET),
\ which keep clean ctor packages and the meaning (a per-class requirement; the
\ promotion gate set, matching the plan's "V1 gate set = default policy"). EVID's slot
\ sums (EVID-CERTIFY--SLOT, 18, ...) were also only SHA-constructible under the old
\ 16-char cap; TF-CTOR-NAME-LIMIT is now 32 (commit e596b0f1), so a full EVID:bundle
\ is constructible by readable name and POLICY:CHECK runs end to end over a real bundle
\ (maki/evidence/policy-e2e-test.f).

require lib/prelude.f
require maki/artifact.f            \ ARTIFACT:EQUAL? - the value-level artifact identity
require maki/schema.f              \ SCHEMA:REGISTER - the § 23.9 schema-id owner package
require maki/evidence/schema.f

\ maki owns -5000..-5099; evidence-policy uses -5098..-5099 (the E-RPT-GATE
\ report.f:34 owning-file convention).
-5098 constant E-EVID-ARTIFACT   \ a required evidence slot names a different artifact than the one under promotion
-5099 constant E-EVID-MISSING    \ a required evidence slot is absent

package POLICY
public

\ grant-proof: the sealed promotion witness; only MINT-GRANT-PROOF produces one.
NEWTYPE grant-proof 0

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

\ gate-set: a promotion policy - one req per evidence class, the policy's schema
\ identity (`pol` participates in the release key, plan §16), and the REQUESTED
\ numeric proof domain (`npol`, maki/numpolicy.f) the golden's ACHIEVED domain must
\ satisfy at promotion. `npol` is the region's requested policy (the skey pol field /
\ maki/sched-key.f REGION-POL) threaded in by the promotion caller; CHECK enforces
\ EVID:GOLD-DOM SATISFIES? npol before minting the grant (see CHECK).
\ Declared through the unified STRUCTURE front end; the header clause and every
\ FIELD line are unchanged, so POLICY-GATE--SET:MAKE / :UNMAKE keep their exact
\ spelling, effect and field order and no call site moves. The first four fields
\ are all `req`, which is precisely where an exchanged pair would be invisible to
\ any value test, so maki/evidence/policy-test.f pins each field NAME to its
\ payload SLOT through the type registry.
STRUCTURE gate-set 0
   FIELD cert req
   FIELD gold req
   FIELD grad req
   FIELD prof req
   FIELD pol  CAD-KIND:schema-id
   FIELD npol NPOL:dom
;STRUCTURE

\ granted: the sealed grant - bound to its artifact, its policy identity, and a
\ private proof token that only POLICY:CHECK can mint. Also a STRUCTURE now, on
\ the same byte-identical terms. Neither record carries `DERIVE eq`, and neither
\ can: a derive clause is refused when ANY field's own family does not itself
\ derive equality, which covers `grant-proof` here and `CAD-KIND:schema-id` in
\ gate-set above (both arity-0 NEWTYPEs). A grant has no equality semantics to
\ offer anyway - its meaning is that it EXISTS, minted by the one word allowed to
\ mint it.
STRUCTURE granted 0
   FIELD art CAD-KIND:artifact-id
   FIELD pol CAD-KIND:schema-id
   FIELD tok grant-proof
;STRUCTURE

private

\ MINT-GRANT-PROOF: the ONLY producer of a grant-proof cell, invoked by CHECK
\ after the value-level binding held (the maki/target/target.f:54 mint pattern).
TRUSTED: MINT-GRANT-PROOF ( -- grant-proof )  0 ;

\ Schema-id minting now lives in its § 23.9 owner package (maki/schema.f): the
\ former private RAW>SCHEMA-ID placeholder is RETIRED here and POLICY:SCHEMA
\ (below) delegates to SCHEMA:REGISTER, so no schema-id refinement is owned here.

\ Artifact-id equality is now ARTIFACT:EQUAL? (maki/artifact.f), which owns the
\ artifact-id raw projection with the registry - retiring policy.f's former AID>RAW /
\ AID= boundary.

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

\ GOLD-SLOT-ERR: SLOT-ERR extended with the numeric-policy check for the golden slot.
\ A present, binding golden must BOTH name the artifact under promotion (art-ok) AND
\ have an achieved numeric domain that satisfies the requested policy (num-ok); the
\ artifact refusal has priority over the numeric refusal. Absence is exactly SLOT-ERR.
: GOLD-SLOT-ERR ( bool bool bool req -- n )   \ present? art-binds? num-satisfies? req
   {: p:bool aok:bool nok:bool r:req :}
   p if
      r REQ-BINDS-IF-PRESENT? if
         aok if  nok if 0 else E-NPOL-APPROX then  else E-EVID-ARTIFACT then
      else 0 then
   else
      r REQ-MUST-EXIST? if E-EVID-MISSING else 0 then
   then ;

\ ---- artifact projections of each evidence class (drop the class proof token) --
: CERT-ART ( EVID:certified -- CAD-KIND:artifact-id )     EVID-CERTIFIED:UNMAKE drop ;
: GOLD-ART ( EVID:golden -- CAD-KIND:artifact-id )        EVID-GOLDEN:UNMAKE drop drop drop drop ;
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
      certify-got  OF CERT-ART a ARTIFACT:EQUAL? true swap ENDOF
      certify-none OF false false ENDOF
   ;MATCH  r SLOT-ERR ;
\ GOLD-VERDICT also enforces the numeric policy: a present golden's achieved domain
\ (EVID:GOLD-DOM) must SATISFY the requested `need` policy, else E-NPOL-APPROX. The
\ got arm computes (present art-ok num-ok) for GOLD-SLOT-ERR; the none arm is absent.
: GOLD-VERDICT ( EVID:golden-slot CAD-KIND:artifact-id req NPOL:dom -- n )
   {: a:CAD-KIND:artifact-id r:req need:NPOL:dom :}
   MATCH golden-slot
      golden-got  OF
         dup  EVID:GOLD-DOM need NPOL:SATISFIES?   \ ( golden num-ok )
         swap GOLD-ART a ARTIFACT:EQUAL?           \ ( num-ok art-ok )
         swap true -rot                            \ ( present=true art-ok num-ok )
      ENDOF
      golden-none OF false false false ENDOF       \ ( present=false art-ok num-ok )
   ;MATCH  r GOLD-SLOT-ERR ;
: GRAD-VERDICT ( EVID:gradcheck-slot CAD-KIND:artifact-id req -- n )
   {: a:CAD-KIND:artifact-id r:req :}
   MATCH gradcheck-slot
      gradcheck-got  OF GRAD-ART a ARTIFACT:EQUAL? true swap ENDOF
      gradcheck-none OF false false ENDOF
   ;MATCH  r SLOT-ERR ;
: PROF-VERDICT ( EVID:profile-slot CAD-KIND:artifact-id req -- n )
   {: a:CAD-KIND:artifact-id r:req :}
   MATCH profile-slot
      profile-got  OF PROF-ART a ARTIFACT:EQUAL? true swap ENDOF
      profile-none OF false false ENDOF
   ;MATCH  r SLOT-ERR ;

public

\ DEFAULT-POLICY: EXACTLY the V1 inference gate set (maki/cad.f:1019-1030) -
\ certify + golden block on pass, gradcheck blocks only a real fail
\ (required-when-supported: absent/unsupported clears like V1 not-run), profile is
\ mandatory-to-run but non-blocking (required-recorded). `pol` is the caller's
\ policy schema identity; `need` is the region's REQUESTED numeric proof domain
\ (maki/sched-key.f REGION-POL) the golden's achieved domain must satisfy.
: DEFAULT-POLICY ( CAD-KIND:schema-id NPOL:dom -- gate-set )
   {: pol:CAD-KIND:schema-id need:NPOL:dom :}
   POLICY-REQ:REQUIRED-BLOCKING
   POLICY-REQ:REQUIRED-BLOCKING
   POLICY-REQ:REQUIRED-WHEN-SUPPORTED
   POLICY-REQ:REQUIRED-RECORDED
   pol
   need
   POLICY-GATE--SET:MAKE ;

\ SCHEMA: the canonical identity of the V1 promotion-policy schema. The schema
\ DEFINITION identity now lives in its § 23.9 owner package (maki/schema.f):
\ SCHEMA:REGISTER interns the canonical, version-independent schema NAME and content-
\ addresses it, so equal names share one id. This word is the default policy's
\ interned schema id (retiring the former POLICY-local RAW>SCHEMA-ID placeholder and
\ the test-only T>SID fabrication); the schema-id mint stays PRIVATE to package SCHEMA.
: SCHEMA ( -- CAD-KIND:schema-id )  s" promotion-policy" SCHEMA:REGISTER ;

\ CHECK: the single sealed site of value-level artifact binding AND numeric-policy
\ enforcement. On a clean policy pass it mints and returns the grant; on refusal it
\ THROWs the first refusal code (cert > gold > grad > prof priority), exactly as V1
\ PROMOTE-REPORT throws E-CAD-GATE (maki/cad.f:1036). The bundle is DEEPEST so it is
\ UNMAKEd only after the single-cell operands become locals; the four verdict codes
\ accumulate through the return stack because a tagged-sum slot cannot rest under a
\ partial result. The artifact under promotion is READ from the ART:built witness
\ (its `art` field), which also proves the artifact was actually built - a permissive
\ policy over an empty bundle cannot grant for a never-built artifact.
\ NUMERIC POLICY (seam rationale): the golden verdict additionally enforces the
\ golden's ACHIEVED domain (EVID:GOLD-DOM) SATISFIES? the gate-set's requested `npol`
\ (E-NPOL-APPROX). This lives INSIDE CHECK, before the grant is minted, precisely to
\ keep the sealed-grant discipline: POLICY:granted exists ONLY when the numeric
\ policy also held, so a downstream ART:PROMOTE consuming the grant never promotes an
\ approximate golden under a stricter request. Enforcing at the promote seam instead
\ would mint the grant first (numeric unchecked) and break that invariant.
: CHECK ( EVID:bundle ART:built gate-set -- POLICY:granted )
   POLICY-GATE--SET:UNMAKE
      {: cReq:req gReq:req grReq:req pReq:req sid:CAD-KIND:schema-id need:NPOL:dom :}
   ART-BUILT:UNMAKE drop            \ built -> its artifact id (drop the build-proof)
   {: a:CAD-KIND:artifact-id :}      \ pop the artifact under promotion, leaving  bundle
   EVID-BUNDLE:UNMAKE               \ stack: certify-slot golden-slot gradcheck-slot profile-slot
   a pReq  PROF-VERDICT >r
   a grReq GRAD-VERDICT >r
   a gReq  need GOLD-VERDICT >r     \ golden also enforces GOLD-DOM SATISFIES? need (E-NPOL-APPROX)
   a cReq  CERT-VERDICT
   r> FIRST-ERR r> FIRST-ERR r> FIRST-ERR
   dup 0= if
      drop  a sid MINT-GRANT-PROOF POLICY-GRANTED:MAKE
   else
      throw
   then ;
;package
