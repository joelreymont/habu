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
\ SCOPE / DEFERRED: POLICY:CHECK's END-TO-END execution over a real EVID:bundle is
\ NOT run here: EVID's slot sums (certify-slot ...) escape to opaque SHA-named
\ constructor packages (TF-CTOR-NAME-LIMIT=16, src/core/type-family.f:604), so a
\ bundle is not constructible in readable source, and there are no public producers
\ for CAD-KIND:artifact-id / ART:built. Instead CHECK is pinned by a type candidate
\ (its plumbing is checker-verified by policy.f loading) and its value-level cores -
\ AID= (artifact-id equality) and SLOT-ERR (the whole requirement decision table) -
\ are executed white-box below, proving the exact refusal codes. See policy.f DEV 3.
\ The two test-local fabrication mints (T>AID / T>SID) exist ONLY because the id
\ producers do not yet; they are the named tested boundary for that gap.

require lib/test.f
require test/checker-assert.f
require maki/evidence/policy.f

\ ---- white-box probes: reopen POLICY to fabricate ids and drive private cores --
package POLICY
private
\ Test-only artifact-id / schema-id fabrication (no public producer exists yet).
TRUSTED: T>AID ( n -- CAD-KIND:artifact-id )  ;
TRUSTED: T>SID ( n -- CAD-KIND:schema-id )    ;
public

\ AID= identity boundary: same raw -> equal, different raw -> not equal.
: PROBE-AID-EQ  ( -- bool )  1 T>AID 1 T>AID AID= ;
: PROBE-AID-NE  ( -- bool )  1 T>AID 2 T>AID AID= ;

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

\ AID= composed through a slot verdict: golden evidence naming artifact B refused
\ for artifact A under a binding class is E-EVID-ARTIFACT (the executed value fact).
: PROBE-VALUE-MISMATCH ( -- n )  true  1 T>AID 2 T>AID AID= POLICY-REQ:REQUIRED-BLOCKING SLOT-ERR ;
: PROBE-VALUE-BIND     ( -- n )  true  1 T>AID 1 T>AID AID= POLICY-REQ:REQUIRED-BLOCKING SLOT-ERR ;

\ DEFAULT-POLICY is EXACTLY the V1 inference gate set (maki/cad.f:1019-1030).
: PROBE-DEFAULT-V1 ( -- bool )
   0 T>SID DEFAULT-POLICY POLICY-GATE--SET:UNMAKE drop   \ drop the schema id
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
s" PG-OK-CHECK ( EVID:bundle ART:built CAD-KIND:artifact-id POLICY:gate-set -- POLICY:granted ) POLICY:CHECK"
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
s" PG-BAD-CHECK ( EVID:bundle ART:built CAD-KIND:artifact-id n -- POLICY:granted ) POLICY:CHECK"
   CHECK-QUIET-CANDIDATE! 0 T=

\ ---- executed value-level cores ----------------------------------------------
\ AID= artifact-id equality boundary.
POLICY:PROBE-AID-EQ T-ASSERT
POLICY:PROBE-AID-NE 0= T-ASSERT
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
