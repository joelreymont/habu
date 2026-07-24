\ maki/db/promotion-policy.f - the promotion POLICY value and its content digest
\ (MODEL-CAD-V2-PLAN.md § 23.9 promotion-policy binding + § R7 "artifact<promoted>";
\ dot habu-v2-evidence-promotion-f8312ebe).
\
\ CONCERN: the immutable promotion POLICY value plus its canonical content digest.
\ A policy is ordinary public DATA (an input, not a proof - the maki/evidence/policy.f
\ gate-set precedent): it BINDS the ten identities/thresholds a promotion is granted
\ against - model, weights, target, numeric policy, populations, verifier identity +
\ version, threshold, expiry, and rollback artifact - as TYPED fields over the landed
\ § 23.9 identities (never raw ids). The typestate chain (package PROMOTE,
\ maki/db/promotion.f) is a SEPARATE concern: this file owns only the policy value and
\ its digest; the Candidate->..->Promoted transitions that consume it are not forked here.
\
\ DIGEST-BINDING (the "policy change invalidates promotion" rule). DIGEST-WORDS is the
\ SHA-256 over the policy's canonical serialization - each identity as its owner's
\ cross-process CONTENT KEY (X:KEY>WIRE, the maki/db/artifact.f envelope discipline) and
\ each scalar as a fixed LE64 - projected to four 64-bit words. A promoted value CARRIES
\ these four words (maki/db/promotion.f), so a policy that differs in ANY bound field
\ digests differently and fails PROMOTE:REVALIDATE. Content-addressed by construction:
\ two policies with identical fields digest identically in every process.
\
\ CONSERVATIVE READINGS (grounded in the landed identities, the maki/db/obligation.f
\ flagged-reading precedent):
\   - populations: the plan names evaluation POPULATIONS; there is no population-id owner,
\     so `population` is modeled as CAD-KIND:config-id (the landed identity nearest an
\     evaluation-population / dataset descriptor - the maki/db/obligation.f environment
\     reading). When a population-id owner lands it becomes a projection of it.
\   - verifier versions: producer-id is VERSION-INDEPENDENT (maki/producer.f splits id from
\     version), so a verifier is bound as its identity `verifier` (CAD-KIND:producer-id)
\     PLUS the separate `vversion` scalar the envelope splits out - both digest-covered.
\ maki -> habu only; promotion-policy owns -5606.

require lib/prelude.f
require maki/cad-kinds.f
require maki/artifact.f            \ CAD-KIND:artifact-id KEY>WIRE (model / weights / rollback)
require maki/target/target.f      \ CAD-KIND:target-id KEY>WIRE
require maki/numpolicy.f          \ CAD-KIND:numeric-policy-id KEY>WIRE
require maki/config.f             \ CAD-KIND:config-id KEY>WIRE (population descriptor)
require maki/producer.f           \ CAD-KIND:producer-id KEY>WIRE (verifier identity)

-5606 constant E-PPOL-BUF          \ policy canonical scratch smaller than the serialized bytes (unreachable cap)

package PPOLICY
public

\ ---- the promotion policy value (ten typed fields over the landed identities) ---
\ Named `spec` because `policy` is a reserved type-declaration keyword (the
\ maki/db/obligation.f `independence` precedent); package PPOLICY carries the domain.
\ Fields (declaration order, also the canonical digest order):
\   model      the model identity the promotion is FOR
\   weights    the weights identity
\   target     the deployment target
\   numeric    the required numeric proof domain
\   population the evaluation-population descriptor (conservative reading, see header)
\   verifier   the authorized verifier IDENTITY (version-independent name)
\   vversion   the verifier VERSION (split from identity, § 23.9)
\   threshold  the promotion acceptance threshold
\   expiry     the promotion expiry (a promotion past it is stale)
\   rollback   the rollback artifact identity
STRUCTURE spec 0
   FIELD model      CAD-KIND:artifact-id
   FIELD weights    CAD-KIND:artifact-id
   FIELD target     CAD-KIND:target-id
   FIELD numeric    CAD-KIND:numeric-policy-id
   FIELD population CAD-KIND:config-id
   FIELD verifier   CAD-KIND:producer-id
   FIELD vversion   n
   FIELD threshold  n
   FIELD expiry     n
   FIELD rollback   CAD-KIND:artifact-id
;STRUCTURE

private

32 constant CKW                    \ owner cross-process content-key width (KEY>WIRE)
8  constant U64W                   \ fixed little-endian scalar width
64 constant IDW-CAP                 \ scratch cap for one owner wire form
$400 constant SCRATCH-CAP          \ policy canonical scratch (6 ids + 3 scalars << this)

create PBUF SCRATCH-CAP allot
variable PO
create IDW IDW-CAP allot           \ one owner wire form
create DG CKW allot                \ SHA-256 output

\ ---- fixed little-endian scalar codec -----------------------------------------
: LE-PUT ( n ptr u8 n -- ) {: v:n a:ptr w:n :}
   0 begin dup w < while
      dup {: k:n :}
      v k 8 * rshift $FF and  a k + c!
      1+
   repeat drop ;

: LE-GET ( ptr u8 n -- n ) {: a:ptr w:n :}
   0  0 begin dup w < while
      dup {: k:n :}
      a k + c@ k 8 * lshift  rot or swap
      1+
   repeat drop ;

\ ---- canonical serialization scratch emitter ----------------------------------
: P-RESET ( -- )   0 PO ! ;
: P-ROOM ( n -- ) {: k:n :}   PO @ k + SCRATCH-CAP > if E-PPOL-BUF throw then ;
: P-BYTES ( ptr u8 n -- ) {: a:ptr u:n :}
   u P-ROOM  a PBUF PO @ + u BYTE-COPY  PO @ u + PO ! ;
: P-LE ( n n -- ) {: v:n w:n :}
   w P-ROOM  v PBUF PO @ + w LE-PUT  PO @ w + PO ! ;

\ Each identity is serialized ACROSS its owner boundary as the cross-process content
\ key (total for a valid id); no raw cast, no refinement in PPOLICY.
: P-ART ( CAD-KIND:artifact-id -- )   IDW IDW-CAP ARTIFACT:KEY>WIRE {: w:n :}  IDW w P-BYTES ;
: P-TGT ( CAD-KIND:target-id -- )     IDW IDW-CAP TARGET:KEY>WIRE {: w:n :}    IDW w P-BYTES ;
: P-NPOL ( CAD-KIND:numeric-policy-id -- )  IDW IDW-CAP NPOL:KEY>WIRE {: w:n :}  IDW w P-BYTES ;
: P-CFG ( CAD-KIND:config-id -- )     IDW IDW-CAP CONFIG:KEY>WIRE {: w:n :}    IDW w P-BYTES ;
: P-PRD ( CAD-KIND:producer-id -- )   IDW IDW-CAP PRODUCER:KEY>WIRE {: w:n :}  IDW w P-BYTES ;

\ Serialize the ten field VALUES into PBUF in fixed order (shared by ENCODE-POLICY and
\ BIND so the canonical form has one authority).
: ENCODE-FIELDS ( CAD-KIND:artifact-id CAD-KIND:artifact-id CAD-KIND:target-id CAD-KIND:numeric-policy-id CAD-KIND:config-id CAD-KIND:producer-id n n n CAD-KIND:artifact-id -- )
   {: model:CAD-KIND:artifact-id weights:CAD-KIND:artifact-id target:CAD-KIND:target-id
      numeric:CAD-KIND:numeric-policy-id population:CAD-KIND:config-id
      verifier:CAD-KIND:producer-id vversion:n threshold:n expiry:n
      rollback:CAD-KIND:artifact-id :}
   P-RESET
   model P-ART  weights P-ART  target P-TGT  numeric P-NPOL
   population P-CFG  verifier P-PRD
   vversion U64W P-LE  threshold U64W P-LE  expiry U64W P-LE
   rollback P-ART ;

: ENCODE-POLICY ( spec -- )   PPOLICY-SPEC:UNMAKE ENCODE-FIELDS ;

\ DG-WORDS reads the four LE64 words from the SHA-256 output buffer.
: DG-WORDS ( -- n n n n )
   DG            U64W LE-GET
   DG U64W +     U64W LE-GET
   DG U64W 2 * + U64W LE-GET
   DG U64W 3 * + U64W LE-GET ;

public

\ MK builds an immutable policy from the ten typed fields (a readable ctor wrapper).
: MK ( CAD-KIND:artifact-id CAD-KIND:artifact-id CAD-KIND:target-id CAD-KIND:numeric-policy-id CAD-KIND:config-id CAD-KIND:producer-id n n n CAD-KIND:artifact-id -- spec )
   PPOLICY-SPEC:MAKE ;

\ DIGEST-WORDS is the content digest as four LE64 words of SHA-256(canonical policy).
\ A promoted value carries these; a policy differing in any bound field digests differently.
: DIGEST-WORDS ( spec -- n n n n )
   ENCODE-POLICY  PBUF PO @ DG SHA256  DG-WORDS ;

\ DIGEST-EQ? is content-key equality between two policies (equal iff every bound field matches).
: DIGEST-EQ? ( spec spec -- bool )
   DIGEST-WORDS {: y0:n y1:n y2:n y3:n :}
   DIGEST-WORDS {: x0:n x1:n x2:n x3:n :}
   x0 y0 = x1 y1 = and x2 y2 = and x3 y3 = and ;

\ BIND is the single-cell projection PROMOTE:SATISFY consumes: the model the policy is FOR,
\ its expiry, and its four content-digest words - all from ONE UNMAKE, so a multi-cell spec
\ never has to survive alongside another multi-cell operand on the transition stack.
: BIND ( spec -- CAD-KIND:artifact-id n n n n n )       \ ( model expiry d0 d1 d2 d3 )
   PPOLICY-SPEC:UNMAKE
   {: model:CAD-KIND:artifact-id weights:CAD-KIND:artifact-id target:CAD-KIND:target-id
      numeric:CAD-KIND:numeric-policy-id population:CAD-KIND:config-id
      verifier:CAD-KIND:producer-id vversion:n threshold:n expiry:n
      rollback:CAD-KIND:artifact-id :}
   model weights target numeric population verifier vversion threshold expiry rollback ENCODE-FIELDS
   PBUF PO @ DG SHA256
   model expiry  DG-WORDS ;

;package
