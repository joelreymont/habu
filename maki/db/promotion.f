\ maki/db/promotion.f - the immutable evidence-promotion TYPESTATE
\ (MODEL-CAD-V2-PLAN.md § R7 "Typestate And Evidence Families" -> artifact<promoted>,
\ + § 23.9 proof obligations / independent verifiers; dot habu-v2-evidence-promotion-f8312ebe).
\
\ CONCERN: the Candidate -> Verified -> Measured -> PolicySatisfied -> Promoted staged
\ typestate. Each stage is a DISTINCT typed value DERIVED (never mutated) from the prior
\ plus the evidence that justifies it - a fresh immutable structure, so the prior stage is
\ structurally unchanged. This is a SEPARATE concern from the obligation VALUE + discharge
\ gate (package OBLIG), the applicability CLOSURE (package APPLIC), the promotion POLICY
\ value (package PPOLICY), and the discharge AUTHORITY gate (package DAUTH): PROMOTE COMPOSES
\ all of them into the staged chain and its audit record.
\
\ UNCONSTRUCTIBLE WITHOUT APPLICABLE EVIDENCE (acceptance 1). Each stage is a STRUCTURE sealed
\ by a class-private proof token: every MINT-*-PROOF is PRIVATE (the maki/evidence/policy.f
\ discipline), so a raw n cannot forge a stage and no wrong-stage value stands in for
\ another (the checker rejects it - the static leg). A transition MINTS the next stage ONLY
\ when the obligation is APPLICABLE over the session working set (compose APPLIC:VERDICT):
\ missing / stale / wrong-target evidence makes APPLIC:VERDICT return missing / stale /
\ inapplicable (the TYPED reject a caller reads directly), and the transition REFUSES
\ (E-PROMO-UNAPPLICABLE) so no next stage exists. The stage is therefore UNCONSTRUCTIBLE
\ without applicable evidence, both dynamically (refusal) and statically (sealed mint).
\
\ REFUSAL CHANNEL (the maki/evidence/policy.f deviation, same wall). A stage is a multi-cell
\ structure, so it cannot be the single-cell `ok` payload of a bespoke result sum; the
\ transitions therefore RETURN the next stage directly and signal refusal by THROWing a
\ named code (exactly as POLICY:CHECK throws E-EVID-*). The typed reject the dot names is
\ the APPLIC:applicability sum a caller queries BEFORE the transition (APPLIC:VERDICT); the
\ transition's throw is its enforcement.
\
\ POLICY BINDING + INVALIDATION (acceptance 3). SATISFY binds the PPOLICY digest (four
\ words) into PolicySatisfied, threaded into Promoted; PROMOTE:REVALIDATE recomputes a
\ given policy's digest and compares - the same policy revalidates, any changed field
\ digests differently and fails. It also enforces the policy is FOR the candidate's model
\ (E-PROMO-MODEL) and not past expiry (E-PROMO-EXPIRED).
\
\ AUDIT (acceptance 4). PROMOTE records the EXACT obligation closure at promotion time -
\ each tracked obligation's content-key + its APPLIC verdict - as a canonical descriptor
\ APPENDed to the journal; the Promoted value carries the audit-event-id. REPLAY-DESC$
\ reads the recorded descriptor back and CLOSURE-DESC$ recomputes it: recorded == recomputed.
\
\ maki -> habu only; promotion owns -5609..-5613.

require lib/prelude.f
require maki/cad-kinds.f
require maki/artifact.f              \ CAD-KIND:artifact-id EQUAL? (model identity)
require maki/db/obligation.f        \ OBLIG: obligation / INTERN / KEY>WIRE
require maki/db/evidence-applicability.f   \ APPLIC: RESET / OBLIGATION+ / EVIDENCE+ / CHANGE+ / VERDICT
require maki/db/promotion-policy.f  \ PPOLICY: spec / BIND / DIGEST-WORDS
require maki/journal.f              \ JOURNAL: APPEND / DESC$ (the audit record owner)

-5609 constant E-PROMO-UNAPPLICABLE  \ VERIFY / MEASURE: the obligation is not applicable (no applicable evidence)
-5610 constant E-PROMO-EXPIRED       \ SATISFY: the promotion policy has expired at the given time
-5611 constant E-PROMO-MODEL         \ SATISFY: the policy binds a different model than the candidate
-5612 constant E-PROMO-CAP           \ tracked-obligation closure set over its per-session cap
-5613 constant E-PROMO-BUF           \ closure descriptor scratch smaller than the serialized bytes

package PROMOTE
public

\ ---- sealed stage proof tokens (minted only by the transitions below) ----------
NEWTYPE cand-proof 0
NEWTYPE ver-proof 0
NEWTYPE meas-proof 0
NEWTYPE sat-proof 0
NEWTYPE prom-proof 0

\ ---- the immutable stage values (each derived, never mutated) ------------------
\ candidate/verified/measured carry the model identity threaded through the chain; the
\ policy digest (d0..d3) joins at PolicySatisfied and the audit-event-id at Promoted.
STRUCTURE candidate 0
   FIELD model CAD-KIND:artifact-id
   FIELD tok cand-proof
;STRUCTURE
STRUCTURE verified 0
   FIELD model CAD-KIND:artifact-id
   FIELD tok ver-proof
;STRUCTURE
STRUCTURE measured 0
   FIELD model CAD-KIND:artifact-id
   FIELD tok meas-proof
;STRUCTURE
STRUCTURE satisfied 0
   FIELD model CAD-KIND:artifact-id
   FIELD d0 n  FIELD d1 n  FIELD d2 n  FIELD d3 n
   FIELD tok sat-proof
;STRUCTURE
STRUCTURE promoted 0
   FIELD model CAD-KIND:artifact-id
   FIELD d0 n  FIELD d1 n  FIELD d2 n  FIELD d3 n
   FIELD audit CAD-KIND:audit-event-id
   FIELD tok prom-proof
;STRUCTURE

private
TRUSTED: MINT-CAND-PROOF ( -- cand-proof )  0 ;
TRUSTED: MINT-VER-PROOF ( -- ver-proof )    0 ;
TRUSTED: MINT-MEAS-PROOF ( -- meas-proof )  0 ;
TRUSTED: MINT-SAT-PROOF ( -- sat-proof )    0 ;
TRUSTED: MINT-PROM-PROOF ( -- prom-proof )  0 ;

public
\ ---- stage read surface (UNMAKE a multi-cell structure; never a typed local) ----
: CAND-MODEL ( candidate -- CAD-KIND:artifact-id )
   PROMOTE-CANDIDATE:UNMAKE {: model:CAD-KIND:artifact-id tok:cand-proof :}  model ;
: VER-MODEL ( verified -- CAD-KIND:artifact-id )
   PROMOTE-VERIFIED:UNMAKE {: model:CAD-KIND:artifact-id tok:ver-proof :}  model ;
: MEAS-MODEL ( measured -- CAD-KIND:artifact-id )
   PROMOTE-MEASURED:UNMAKE {: model:CAD-KIND:artifact-id tok:meas-proof :}  model ;

private
\ ---- session working set: tracked obligations + the delegated APPLIC pools -------
\ PROMOTE wraps APPLIC as the promotion working set and ALSO records the tracked
\ obligations (in order) so the audit closure can name them. Evidence + change-set are
\ added straight through to APPLIC.
32 constant OB-CAP
OB-CAP TYPED-BUFFER SESS-OB OBLIG:obligation
variable SESS-N

: SESS-OB@ ( n -- OBLIG:obligation )   SESS-OB @ ;

public
: RESET ( -- )   APPLIC:RESET  0 SESS-N ! ;
: OBLIGATION+ ( OBLIG:obligation -- ) {: o:OBLIG:obligation :}
   o APPLIC:OBLIGATION+
   SESS-N @ OB-CAP >= if E-PROMO-CAP throw then
   o SESS-N @ SESS-OB !
   SESS-N @ 1+ SESS-N ! ;
: EVIDENCE+ ( OBLIG:evidence -- )      APPLIC:EVIDENCE+ ;
: CHANGE+ ( CAD-KIND:artifact-id -- )  APPLIC:CHANGE+ ;

private
\ ---- applicability gate (compose APPLIC:VERDICT) -------------------------------
: APPLICABLE? ( OBLIG:obligation -- bool )
   APPLIC:VERDICT MATCH APPLIC:applicability
      applicable   OF true ENDOF
      stale        OF false ENDOF
      missing      OF false ENDOF
      inapplicable OF false ENDOF
   ;MATCH ;

\ VERDICT-ORD projects an obligation's applicability into a stable audit ordinal.
: VERDICT-ORD ( OBLIG:obligation -- n )
   APPLIC:VERDICT MATCH APPLIC:applicability
      applicable   OF 0 ENDOF
      stale        OF 1 ENDOF
      missing      OF 2 ENDOF
      inapplicable OF 3 ENDOF
   ;MATCH ;

\ ---- canonical audit-closure descriptor ---------------------------------------
32 constant CKW                    \ obligation content-key width (OBLIG:KEY>WIRE)
64 constant OIDW-CAP
$800 constant CBUF-CAP

create CBUF CBUF-CAP allot
variable CO
create OIDW OIDW-CAP allot

: C-RESET ( -- )   0 CO ! ;
: C-ROOM ( n -- ) {: k:n :}   CO @ k + CBUF-CAP > if E-PROMO-BUF throw then ;
: C-BYTES ( ptr u8 n -- ) {: a:ptr u:n :}
   u C-ROOM  a CBUF CO @ + u BYTE-COPY  CO @ u + CO ! ;
: C-U8 ( n -- ) {: v:n :}   1 C-ROOM  v CBUF CO @ + c!  CO @ 1+ CO ! ;

\ CLOSURE-BUILD serialises each tracked obligation's content key + verdict ordinal into
\ CBUF, in tracked (deterministic) order - the EXACT obligation closure at promotion time.
: CLOSURE-BUILD ( -- )
   C-RESET
   0 begin dup SESS-N @ < while
      dup {: k:n :}
      k SESS-OB@ {: o:OBLIG:obligation :}
      o OBLIG:INTERN OIDW OIDW-CAP OBLIG:KEY>WIRE {: w:n :}
      OIDW w C-BYTES
      o VERDICT-ORD C-U8
      1+
   repeat drop ;

public

\ ---- the seed: an immutable Candidate over a model artifact ---------------------
: CANDIDATE ( CAD-KIND:artifact-id -- candidate )
   MINT-CAND-PROOF PROMOTE-CANDIDATE:MAKE ;

\ ---- VERIFY: derive a Verified from a Candidate + an APPLICABLE obligation -------
\ The obligation must be applicable over the session evidence + change-set; otherwise
\ the transition refuses (E-PROMO-UNAPPLICABLE) and no Verified is constructed. A Verified
\ is otherwise unforgeable (sealed ver-proof).
: VERIFY ( candidate OBLIG:obligation -- verified ) {: o:OBLIG:obligation :}
   o APPLICABLE? 0= if E-PROMO-UNAPPLICABLE throw then
   PROMOTE-CANDIDATE:UNMAKE {: model:CAD-KIND:artifact-id tok:cand-proof :}
   model MINT-VER-PROOF PROMOTE-VERIFIED:MAKE ;

\ ---- MEASURE: derive a Measured from a Verified + an APPLICABLE measurement obligation ---
\ Same applicability gate over the (device / performance) measurement obligation.
: MEASURE ( verified OBLIG:obligation -- measured ) {: o:OBLIG:obligation :}
   o APPLICABLE? 0= if E-PROMO-UNAPPLICABLE throw then
   PROMOTE-VERIFIED:UNMAKE {: model:CAD-KIND:artifact-id tok:ver-proof :}
   model MINT-MEAS-PROOF PROMOTE-MEASURED:MAKE ;

\ ---- SATISFY: derive a PolicySatisfied from a Measured + a bound policy ----------
\ The policy must be FOR this model (E-PROMO-MODEL) and not past `now` (E-PROMO-EXPIRED);
\ its four content-digest words are bound into the value (the digest-binding for
\ REVALIDATE). PPOLICY:BIND yields the single-cell inputs so the multi-cell measured is
\ the only structure on the stack when it is UNMAKEd.
: SATISFY ( measured PPOLICY:spec n -- satisfied ) {: now:n :}
   PPOLICY:BIND
   {: pmodel:CAD-KIND:artifact-id pexpiry:n d0:n d1:n d2:n d3:n :}
   PROMOTE-MEASURED:UNMAKE {: mmodel:CAD-KIND:artifact-id tok:meas-proof :}
   pmodel mmodel ARTIFACT:EQUAL? 0= if E-PROMO-MODEL throw then
   now pexpiry >= if E-PROMO-EXPIRED throw then
   mmodel d0 d1 d2 d3 MINT-SAT-PROOF PROMOTE-SATISFIED:MAKE ;

\ ---- PROMOTE: record the audit closure and derive the Promoted value -------------
\ Records the EXACT obligation closure (content keys + verdicts) at promotion time to the
\ journal, minting the audit-event-id the Promoted value carries; the policy digest threads
\ through. A Promoted value is unforgeable (sealed prom-proof).
: PROMOTE ( satisfied -- promoted )
   CLOSURE-BUILD
   CBUF CO @ JOURNAL:APPEND {: audit:CAD-KIND:audit-event-id :}
   PROMOTE-SATISFIED:UNMAKE
   {: model:CAD-KIND:artifact-id d0:n d1:n d2:n d3:n tok:sat-proof :}
   model d0 d1 d2 d3 audit MINT-PROM-PROOF PROMOTE-PROMOTED:MAKE ;

\ ---- Promoted read surface -----------------------------------------------------
: PROM-MODEL ( promoted -- CAD-KIND:artifact-id )
   PROMOTE-PROMOTED:UNMAKE
   {: model:CAD-KIND:artifact-id d0:n d1:n d2:n d3:n audit:CAD-KIND:audit-event-id tok:prom-proof :}
   model ;
: PROM-DIGEST ( promoted -- n n n n )
   PROMOTE-PROMOTED:UNMAKE
   {: model:CAD-KIND:artifact-id d0:n d1:n d2:n d3:n audit:CAD-KIND:audit-event-id tok:prom-proof :}
   d0 d1 d2 d3 ;
: PROM-AUDIT ( promoted -- CAD-KIND:audit-event-id )
   PROMOTE-PROMOTED:UNMAKE
   {: model:CAD-KIND:artifact-id d0:n d1:n d2:n d3:n audit:CAD-KIND:audit-event-id tok:prom-proof :}
   audit ;

\ ---- REVALIDATE: a changed policy invalidates the promotion (digest-bound) --------
\ True iff the given policy digests to the four words the Promoted value carries. The same
\ policy revalidates; any changed bound field digests differently and fails. The multi-cell
\ spec is consumed (top) before the Promoted value.
: REVALIDATE ( promoted PPOLICY:spec -- bool )
   PPOLICY:DIGEST-WORDS {: b0:n b1:n b2:n b3:n :}
   PROM-DIGEST {: a0:n a1:n a2:n a3:n :}
   a0 b0 = a1 b1 = and a2 b2 = and a3 b3 = and ;

\ ---- audit replay --------------------------------------------------------------
\ REPLAY-DESC$ is the closure descriptor RECORDED in the journal at promotion time;
\ CLOSURE-DESC$ RECOMPUTES it from the current session. recorded == recomputed.
: REPLAY-DESC$ ( promoted -- ptr u8 n )   PROM-AUDIT JOURNAL:DESC$ ;
: CLOSURE-DESC$ ( -- ptr u8 n )           CLOSURE-BUILD CBUF CO @ ;

;package
