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
require lib/string.f               \ STR= - the registry pins below compare family and field names
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

\ DEFAULT-POLICY is EXACTLY the V1 inference gate set (maki/cad.f:1019-1030), plus
\ the requested numeric policy carried in the `npol` field (round-trips here).
: PROBE-DEFAULT-V1 ( -- bool )
   SCHEMA NPOL-DOM:RELATIVE DEFAULT-POLICY POLICY-GATE--SET:UNMAKE
   {: cert:req gold:req grad:req prof:req sid:CAD-KIND:schema-id need:NPOL:dom :}
   cert POLICY-REQ:REQUIRED-BLOCKING       POLICY-REQ:EQ
   gold POLICY-REQ:REQUIRED-BLOCKING       POLICY-REQ:EQ and
   grad POLICY-REQ:REQUIRED-WHEN-SUPPORTED POLICY-REQ:EQ and
   prof POLICY-REQ:REQUIRED-RECORDED       POLICY-REQ:EQ and
   need NPOL-DOM:RELATIVE NPOL-DOM:EQ       and ;   \ requested numeric policy round-trips

\ ---- gate-set field-order fixture ---------------------------------------------
\ DEFAULT-POLICY cannot see a reordered gate-set: it binds cert and gold to the
\ SAME req value, so exchanging those two fields is invisible to PROBE-DEFAULT-V1.
\ This fixture gives all four requirement fields DISTINCT values, so a value-visible
\ exchange reports two wrong requirement classes; the field NAME to payload SLOT
\ pins below catch the exchanges values still cannot see.
: PROBE-DISTINCT ( -- gate-set )
   POLICY-REQ:REQUIRED-BLOCKING
   POLICY-REQ:REQUIRED-WHEN-SUPPORTED
   POLICY-REQ:REQUIRED-RECORDED
   POLICY-REQ:INFORMATIONAL
   SCHEMA
   NPOL-DOM:EXACT
   POLICY-GATE--SET:MAKE ;

: PROBE-ORDER ( -- bool )                 \ every field comes back from the slot it went into
   PROBE-DISTINCT POLICY-GATE--SET:UNMAKE
   {: cert:req gold:req grad:req prof:req sid:CAD-KIND:schema-id need:NPOL:dom :}
   cert POLICY-REQ:REQUIRED-BLOCKING       POLICY-REQ:EQ
   gold POLICY-REQ:REQUIRED-WHEN-SUPPORTED POLICY-REQ:EQ and
   grad POLICY-REQ:REQUIRED-RECORDED       POLICY-REQ:EQ and
   prof POLICY-REQ:INFORMATIONAL           POLICY-REQ:EQ and
   sid SCHEMA SCHEMA:EQUAL? and
   need NPOL-DOM:EXACT NPOL-DOM:EQ and ;
;package

\ ---- record-shape reflection ---------------------------------------------------
\ A STRUCTURE publishes its fields as type-registry rows keyed (family, no-variant).
\ These helpers read those rows through the public read-only registry axioms, so the
\ pins at the bottom of this file can state the migrated records' field NAME to
\ payload SLOT mapping - the one thing a positional MAKE/UNMAKE round-trip over
\ same-typed fields cannot observe. The helpers live in a test-owned package because
\ a test file may not define global words.
package POLICY-REC
private

: FAM-CTOR? ( n ptr u8 n -- bool ) {: fam:n pa:ptr pu:n :}
   fam TFAM-VAR-COUNT@ 0 <= if false exit then
   fam TFAM-VAR-START@ SUMV-CTOR-PKG$ pa pu STR= ;

: FAM-HIT? ( n ptr u8 n ptr u8 n -- bool ) {: fam:n ta:ptr tu:n pa:ptr pu:n :}
   fam TFAM-NAME$ ta tu STR= fam pa pu FAM-CTOR? and ;

\ A family is identified by its tail plus the constructor package its generated
\ operations carry - exactly the (package, tail) pair that owns family identity.
: FAM-N ( ptr u8 n ptr u8 n -- n ) {: ta:ptr tu:n pa:ptr pu:n :}
   0
   TFAM-N@ 0 ?do
      i ta tu pa pu FAM-HIT? if 1+ then
   loop ;

: FAM-ID ( ptr u8 n ptr u8 n -- n ) {: ta:ptr tu:n pa:ptr pu:n :}   \ family id, or -1
   TFAM-N@ 0 ?do
      i ta tu pa pu FAM-HIT? if i unloop exit then
   loop -1 ;

\ FAM-ID answers -1 for a family that is not registered and the registry readers
\ take a live id, so every read refuses the sentinel first: a missing family must
\ report a wrong number, never read a record that is not there.
: LIVE-KIND ( n -- n ) {: fam:n :}
   fam 0 < if -1 exit then
   fam TFAM-KIND@ ;

: LIVE-WIDTH ( n -- n ) {: fam:n :}
   fam 0 < if -1 exit then
   fam TFAM-WIDTH@ ;

: FLD-N ( n -- n ) {: fam:n :}                    \ record field rows this family owns
   0
   TYPE-FIELD:COUNT 0 ?do
      i TYPE-FIELD:FAMILY@ fam =
      i TYPE-FIELD:VARIANT@ TYPE-FIELD:NO-VARIANT = and if 1+ then
   loop ;

public

: REC-FAMS ( ptr u8 n ptr u8 n -- n )    FAM-N ;
: REC-KIND ( ptr u8 n ptr u8 n -- n )    FAM-ID LIVE-KIND ;
: REC-WIDTH ( ptr u8 n ptr u8 n -- n )   FAM-ID LIVE-WIDTH ;
: REC-FLDS ( ptr u8 n ptr u8 n -- n )    FAM-ID FLD-N ;

: REC-SLOT ( ptr u8 n ptr u8 n ptr u8 n -- n )    \ payload slot of a named field, -1 when absent
   {: ta:ptr tu:n pa:ptr pu:n na:ptr nu:n :}
   ta tu pa pu FAM-ID {: fam:n :}
   fam TYPE-FIELD:NO-VARIANT na nu TYPE-FIELD:FIND 0= if drop -1 exit then
   TYPE-FIELD:SLOT@ ;

\ the two (tail, constructor package) identities this file pins
: GATE$ ( -- ptr u8 n ptr u8 n )    s" gate-set" s" POLICY-GATE--SET" ;
: GRANT$ ( -- ptr u8 n ptr u8 n )   s" granted" s" POLICY-GRANTED" ;

;package

T-RESET

\ ---- positive controls: the public surface certifies -------------------------
s" PG-OK-DEFAULT ( CAD-KIND:schema-id NPOL:dom -- POLICY:gate-set ) POLICY:DEFAULT-POLICY"
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
\ ...and every gate-set field round-trips from its own slot under four DISTINCT
\ requirement classes, which the V1 gate set (cert = gold) cannot show.
POLICY:PROBE-ORDER T-ASSERT

\ ---- gate-set and granted as STRUCTUREs ----------------------------------------
\ Both records moved from the legacy PRODUCT definer to the unified STRUCTURE front
\ end with byte-identical FIELD lines. These pins are the migration's identity
\ proof: the generated constructor pair keeps its exact spelling and effect, so no
\ call site moves. A YES demands verdict -1 and the checker answers 1 for a name it
\ cannot resolve, so -1 proves it resolved EXACTLY this name.
s" PG-MK ( POLICY:req POLICY:req POLICY:req POLICY:req CAD-KIND:schema-id NPOL:dom -- POLICY:gate-set ) POLICY-GATE--SET:MAKE"
   CHECK-QUIET-CANDIDATE! -1 T=
s" PG-UN ( POLICY:gate-set -- POLICY:req POLICY:req POLICY:req POLICY:req CAD-KIND:schema-id NPOL:dom ) POLICY-GATE--SET:UNMAKE"
   CHECK-QUIET-CANDIDATE! -1 T=
s" PG-GUN ( POLICY:granted -- CAD-KIND:artifact-id CAD-KIND:schema-id POLICY:grant-proof ) POLICY-GRANTED:UNMAKE"
   CHECK-QUIET-CANDIDATE! -1 T=

\ forge negatives on gate-set: the six cells are mandatory and exact, a raw cell
\ cannot stand in for a requirement class or an identity, the two trailing
\ single-cell fields cannot trade places, and the record is not a bare scalar.
s" PG-MK-RAW ( n POLICY:req POLICY:req POLICY:req CAD-KIND:schema-id NPOL:dom -- POLICY:gate-set ) POLICY-GATE--SET:MAKE"
   CHECK-QUIET-CANDIDATE! 0 T=
s" PG-MK-RAWID ( POLICY:req POLICY:req POLICY:req POLICY:req n NPOL:dom -- POLICY:gate-set ) POLICY-GATE--SET:MAKE"
   CHECK-QUIET-CANDIDATE! 0 T=
s" PG-MK-TAILSWAP ( POLICY:req POLICY:req POLICY:req POLICY:req NPOL:dom CAD-KIND:schema-id -- POLICY:gate-set ) POLICY-GATE--SET:MAKE"
   CHECK-QUIET-CANDIDATE! 0 T=
s" PG-MK-SHORT ( POLICY:req POLICY:req POLICY:req CAD-KIND:schema-id NPOL:dom -- POLICY:gate-set ) POLICY-GATE--SET:MAKE"
   CHECK-QUIET-CANDIDATE! 0 T=
s" PG-MK-LONG ( POLICY:req POLICY:req POLICY:req POLICY:req POLICY:req CAD-KIND:schema-id NPOL:dom -- POLICY:gate-set ) POLICY-GATE--SET:MAKE"
   CHECK-QUIET-CANDIDATE! 0 T=
s" PG-UN-BARE ( POLICY:gate-set -- n ) POLICY-GATE--SET:UNMAKE"
   CHECK-QUIET-CANDIDATE! 0 T=
s" PG-MK-BARE ( n -- POLICY:gate-set ) POLICY-GATE--SET:MAKE"
   CHECK-QUIET-CANDIDATE! 0 T=
\ a foreign id family cannot fill the policy-schema slot even at equal width.
s" PG-MK-FGNID ( POLICY:req POLICY:req POLICY:req POLICY:req CAD-KIND:artifact-id NPOL:dom -- POLICY:gate-set ) POLICY-GATE--SET:MAKE"
   CHECK-QUIET-CANDIDATE! 0 T=
\ the sealed grant never unmakes into raw cells - that would hand out its proof.
s" PG-GUN-RAW ( POLICY:granted -- CAD-KIND:artifact-id CAD-KIND:schema-id n ) POLICY-GRANTED:UNMAKE"
   CHECK-QUIET-CANDIDATE! 0 T=
\ ...and a foreign class's proof token cannot be read out of it either.
s" PG-GUN-FGN ( POLICY:granted -- CAD-KIND:artifact-id CAD-KIND:schema-id EVID:certify-proof ) POLICY-GRANTED:UNMAKE"
   CHECK-QUIET-CANDIDATE! 0 T=

\ ---- the declared field names sit at the declared payload slots -----------------
\ gate-set's first four fields are all `req`, so an exchanged pair of them is
\ invisible to any value the record can produce; granted's proof field is what makes
\ the record sealed. Both are pinned by NAME to SLOT through the read-only type
\ registry (the same axioms tools/public-signatures-core.f reads), because a
\ keyword-swap migration has to prove nothing moved, not merely that it still runs.
POLICY-REC:GATE$ POLICY-REC:REC-FAMS 1 T=
POLICY-REC:GRANT$ POLICY-REC:REC-FAMS 1 T=
POLICY-REC:GATE$ POLICY-REC:REC-KIND POLICY-REC:GRANT$ POLICY-REC:REC-KIND T=   \ one record kind
POLICY-REC:GATE$ POLICY-REC:REC-WIDTH 6 T=      \ six single-cell fields, no tag
POLICY-REC:GRANT$ POLICY-REC:REC-WIDTH 3 T=
POLICY-REC:GATE$ POLICY-REC:REC-FLDS 6 T=       \ exactly six named cells, no more
POLICY-REC:GRANT$ POLICY-REC:REC-FLDS 3 T=
POLICY-REC:GATE$ s" cert" POLICY-REC:REC-SLOT 0 T=
POLICY-REC:GATE$ s" gold" POLICY-REC:REC-SLOT 1 T=
POLICY-REC:GATE$ s" grad" POLICY-REC:REC-SLOT 2 T=
POLICY-REC:GATE$ s" prof" POLICY-REC:REC-SLOT 3 T=
POLICY-REC:GATE$ s" pol" POLICY-REC:REC-SLOT 4 T=
POLICY-REC:GATE$ s" npol" POLICY-REC:REC-SLOT 5 T=
POLICY-REC:GRANT$ s" art" POLICY-REC:REC-SLOT 0 T=
POLICY-REC:GRANT$ s" pol" POLICY-REC:REC-SLOT 1 T=
POLICY-REC:GRANT$ s" tok" POLICY-REC:REC-SLOT 2 T=
\ an undeclared name resolves to no slot in either record.
POLICY-REC:GATE$ s" tok" POLICY-REC:REC-SLOT -1 T=
POLICY-REC:GRANT$ s" npol" POLICY-REC:REC-SLOT -1 T=

T-REPORT
