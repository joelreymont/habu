\ policy-test.f - R7 promotion-policy acceptance suite (sub-dot 3
\ v2-promotion-policy, dot habu-v2-typestate-promotion-d539e648).
\
\ Pins the addendum's promotion layer (MODEL-CAD-V2-PLAN.md § Promotion-policy
\ products, fixtures at :1832-1876) against the landed policy.f. Every negative is
\ paired with a resolving positive control (per LESSONS). CHECK-QUIET-CANDIDATE!
\ returns -1 certified / 0 type-reject / 1 unresolvable (sealed private word not
\ visible); both 0 and 1 are "fails to certify" (test/checker-assert.f).
\
\ How this closes the V1 census (MODEL-CAD-V2-PLAN.md:1564-1583) for promotion:
\ - probe 2 (forgeable promotion readout, :1571-1575): POLICY:granted is a SEALED
\   product - its grant-proof token is minted only by the private MINT-GRANT-PROOF,
\   inside POLICY:CHECK. PG-BAD-RAW (raw n token) and PG-BAD-CLASS (an EVID proof
\   token) type-reject; PG-BAD-MINT shows the mint is unresolvable outside POLICY,
\   so no code can fabricate a grant around CHECK.
\ - probe 3 (nothing ties a verdict to the artifact, :1576-1583): POLICY:CHECK is
\   the ONE value-level artifact binding site. The executed probes below prove a
\   present evidence whose art != the artifact under promotion refuses with
\   E-EVID-ARTIFACT (plan:1866-1869) and an absent required slot with E-EVID-MISSING,
\   while a matching bind grants.
\
\ SCOPE: this suite pins POLICY's checked plumbing (type candidates), its private
\ decision cores (SLOT-ERR - the whole requirement decision table), and DEFAULT-POLICY
\ against the V1 gate set. The values it needs are now built through the PUBLIC
\ producers landed by dot habu-public-producers-for-7084d81c - artifact ids from
\ ARTIFACT:REGISTER (maki/artifact.f) and the policy schema id from POLICY:SCHEMA - so
\ the former test-only fabrication mints (T>AID / T>SID) are retired. POLICY:CHECK's
\ END-TO-END execution over a real EVID:bundle now runs in maki/evidence/policy-e2e-test.f
\ (a real bundle is constructible since TF-CTOR-NAME-LIMIT rose to 32).

require lib/test.f
require test/checker-assert.f
require maki/artifact.f
require maki/evidence/policy.f

\ ---- white-box probes: reopen POLICY to drive its private decision cores -------
\ Artifact ids are real (ARTIFACT:REGISTER); only SLOT-ERR / the schema equality are
\ package-private, so the probes live inside a POLICY reopen.
package POLICY
public

\ SLOT-ERR decision table (present? match? req -> refusal code).
\ Present + mismatch under a binding class is THE plan:1866-1869 fact.
: PROBE-MISMATCH-BLOCK ( -- n )  true  false POLICY-REQ:REQUIRED-BLOCKING       SLOT-ERR ;
: PROBE-MISMATCH-WS    ( -- n )  true  false POLICY-REQ:REQUIRED-WHEN-SUPPORTED SLOT-ERR ;
: PROBE-MISMATCH-REC   ( -- n )  true  false POLICY-REQ:REQUIRED-RECORDED       SLOT-ERR ;
: PROBE-MISMATCH-INFO  ( -- n )  true  false POLICY-REQ:INFORMATIONAL           SLOT-ERR ;
: PROBE-BIND-OK        ( -- n )  true  true  POLICY-REQ:REQUIRED-BLOCKING       SLOT-ERR ;
: PROBE-ABSENT-BLOCK   ( -- n )  false false POLICY-REQ:REQUIRED-BLOCKING       SLOT-ERR ;
: PROBE-ABSENT-REC     ( -- n )  false false POLICY-REQ:REQUIRED-RECORDED       SLOT-ERR ;
: PROBE-ABSENT-WS      ( -- n )  false false POLICY-REQ:REQUIRED-WHEN-SUPPORTED SLOT-ERR ;
: PROBE-ABSENT-INFO    ( -- n )  false false POLICY-REQ:INFORMATIONAL           SLOT-ERR ;

\ ARTIFACT:EQUAL? composed through a slot verdict: a present value naming artifact B
\ refused for artifact A under a binding class is E-EVID-ARTIFACT (the executed value
\ fact); the same artifact binds. The ids come from the real registry.
: PROBE-VALUE-MISMATCH ( -- n )
   true  s" art-a" ARTIFACT:REGISTER s" art-b" ARTIFACT:REGISTER ARTIFACT:EQUAL?
   POLICY-REQ:REQUIRED-BLOCKING SLOT-ERR ;
: PROBE-VALUE-BIND     ( -- n )
   true  s" art-a" ARTIFACT:REGISTER s" art-a" ARTIFACT:REGISTER ARTIFACT:EQUAL?
   POLICY-REQ:REQUIRED-BLOCKING SLOT-ERR ;

\ DEFAULT-POLICY is EXACTLY the V1 inference gate set (maki/cad.f:1019-1030).
: PROBE-DEFAULT-V1 ( -- bool )
   SCHEMA DEFAULT-POLICY POLICY-GATE--SET:UNMAKE drop   \ drop the schema id
   {: cert:req gold:req grad:req prof:req :}
   cert POLICY-REQ:REQUIRED-BLOCKING       POLICY-REQ:EQ
   gold POLICY-REQ:REQUIRED-BLOCKING       POLICY-REQ:EQ and
   grad POLICY-REQ:REQUIRED-WHEN-SUPPORTED POLICY-REQ:EQ and
   prof POLICY-REQ:REQUIRED-RECORDED       POLICY-REQ:EQ and ;
;package

T-RESET

\ ---- positive controls: the public surface certifies -------------------------
s" PG-OK-DEFAULT ( CAD-KIND:schema-id -- POLICY:gate-set ) POLICY:DEFAULT-POLICY"
   CHECK-QUIET-CANDIDATE! -1 T=
s" PG-OK-CHECK ( EVID:bundle ART:built POLICY:gate-set -- POLICY:granted ) POLICY:CHECK"
   CHECK-QUIET-CANDIDATE! -1 T=
s" PG-OK-GRANT ( CAD-KIND:artifact-id CAD-KIND:schema-id POLICY:grant-proof -- POLICY:granted ) POLICY-GRANTED:MAKE"
   CHECK-QUIET-CANDIDATE! -1 T=
s" PG-OK-REQ-EQ ( POLICY:req POLICY:req -- bool ) POLICY-REQ:EQ"
   CHECK-QUIET-CANDIDATE! -1 T=

\ ---- missing-gate + forge negatives (plan:1871-1876) -------------------------
\ TS-BAD-GRANT: a raw n cannot fill the sealed grant-proof token slot -> reject.
s" TS-BAD-GRANT ( CAD-KIND:artifact-id CAD-KIND:schema-id n -- POLICY:granted ) POLICY-GRANTED:MAKE"
   CHECK-QUIET-CANDIDATE! 0 T=
\ a foreign (EVID) proof token cannot stand in for a grant-proof -> reject.
s" PG-BAD-CLASS ( CAD-KIND:artifact-id CAD-KIND:schema-id EVID:certify-proof -- POLICY:granted ) POLICY-GRANTED:MAKE"
   CHECK-QUIET-CANDIDATE! 0 T=
\ the artifact-id and schema-id slots are distinct id families -> a swap rejects.
s" PG-BAD-IDSWAP ( CAD-KIND:schema-id CAD-KIND:artifact-id POLICY:grant-proof -- POLICY:granted ) POLICY-GRANTED:MAKE"
   CHECK-QUIET-CANDIDATE! 0 T=
\ the grant-proof mint is PRIVATE to POLICY: unresolvable outside its owner (1),
\ so no caller can mint a grant around POLICY:CHECK.
s" PG-BAD-MINT ( CAD-KIND:artifact-id CAD-KIND:schema-id -- POLICY:granted ) MINT-GRANT-PROOF POLICY-GRANTED:MAKE"
   CHECK-QUIET-CANDIDATE! 1 T=
\ CHECK requires a real gate-set: a raw n in that slot rejects.
s" PG-BAD-CHECK ( EVID:bundle ART:built n -- POLICY:granted ) POLICY:CHECK"
   CHECK-QUIET-CANDIDATE! 0 T=

\ ---- executed value-level cores ----------------------------------------------
\ (artifact-id equality itself is exercised in maki/artifact-test.f; here it is
\ composed through the slot decision table with real registered ids.)
\ the plan:1866-1869 fact: present evidence naming a different artifact refuses.
POLICY:PROBE-VALUE-MISMATCH E-EVID-ARTIFACT T=
POLICY:PROBE-VALUE-BIND     0 T=
\ SLOT-ERR decision table, exhaustive over the interesting cells.
POLICY:PROBE-MISMATCH-BLOCK E-EVID-ARTIFACT T=
POLICY:PROBE-MISMATCH-WS    E-EVID-ARTIFACT T=
POLICY:PROBE-MISMATCH-REC   E-EVID-ARTIFACT T=
POLICY:PROBE-MISMATCH-INFO  0 T=
POLICY:PROBE-BIND-OK        0 T=
POLICY:PROBE-ABSENT-BLOCK   E-EVID-MISSING T=
POLICY:PROBE-ABSENT-REC     E-EVID-MISSING T=
POLICY:PROBE-ABSENT-WS      0 T=
POLICY:PROBE-ABSENT-INFO    0 T=
\ DEFAULT-POLICY is EXACTLY the V1 gate set.
POLICY:PROBE-DEFAULT-V1 T-ASSERT

T-REPORT
