\ maki/db/promotion-test.f - acceptance for the evidence-promotion TYPESTATE
\ (maki/db/promotion.f; dot habu-v2-evidence-promotion-f8312ebe).
\
\ Each dot acceptance item, test-proven:
\   ACCEPTANCE 1 (missing/stale/wrong-target -> unconstructible):
\     PV-*  : the TYPED reject - APPLIC:VERDICT returns applicable/stale/missing/inapplicable
\             for present / changed / wrong-target / wrong-domain evidence.
\     *-CODE: the CONSTRUCTOR refusal - VERIFY / MEASURE THROW E-PROMO-UNAPPLICABLE, so no
\             next stage is constructed when the obligation is not applicable.
\     TS-*  : the STATIC leg (cad-kinds verdict pattern) - a raw n / wrong-stage value cannot
\             stand where a sealed stage is required (verdict 0), and the private stage mint
\             is unresolvable outside the package (verdict 1 / search-wl).
\   ACCEPTANCE 2 (transitions never mutate the candidate): ID-* - the model identity threads
\             Candidate -> Promoted unchanged; a Candidate rebuilt from the model is unchanged.
\   ACCEPTANCE 3 (policy change invalidates): REVAL-SAME true / REVAL-DIFF false (digest-bound).
\   ACCEPTANCE 4 (audit records exact closure): AUDIT-MATCH - the recorded closure descriptor
\             equals the recomputed one (recorded set == recomputed set).
\   ACCEPTANCE 5 is the conjunction of 1-4 above.
\ Also SATISFY's model / expiry guards. All ADT values are produced/consumed inside colon words.

require lib/test.f
require lib/string.f
require maki/db/promotion.f
require maki/db/promotion-policy.f
require maki/db/obligation.f
require maki/db/evidence-applicability.f
require maki/artifact.f
require maki/target/target.f
require maki/numpolicy.f
require maki/config.f
require maki/producer.f

package PROMO-TEST

\ ---- shared identities ---------------------------------------------------------
: MODEL ( -- CAD-KIND:artifact-id )   s" promo-test/model-1" ARTIFACT:REGISTER ;
: MODEL2 ( -- CAD-KIND:artifact-id )  s" promo-test/model-2" ARTIFACT:REGISTER ;
: WTS ( -- CAD-KIND:artifact-id )     s" promo-test/weights-1" ARTIFACT:REGISTER ;
: RB ( -- CAD-KIND:artifact-id )      s" promo-test/rollback-1" ARTIFACT:REGISTER ;
: TGT ( -- CAD-KIND:target-id )       TARGET:SM87 ;
: NUM ( -- CAD-KIND:numeric-policy-id ) NPOL-DOM:EXACT NPOL:REGISTER ;
: POP ( -- CAD-KIND:config-id )       s" promo-test/pop-1" CONFIG:REGISTER ;
: ENV ( -- CAD-KIND:config-id )       s" promo-test/env" CONFIG:REGISTER ;
: PROD ( -- CAD-KIND:producer-id )    s" promo-test/agent" PRODUCER:REGISTER ;
: VER ( -- CAD-KIND:producer-id )     s" promo-test/verifier" PRODUCER:REGISTER ;

\ ---- obligations + evidence ----------------------------------------------------
\ Verify obligation: semantic equivalence, exact domain, differential-exec, self-verify.
: OV ( -- OBLIG:obligation )
   OBLIG:NEW  MODEL OBLIG:SUBJECT
   OBLIG-RELATION:SEMANTIC-EQUIV OBLIG:RELATION
   OBLIG-DOMAIN:EXACT OBLIG:DOMAIN
   OBLIG-INDEPENDENCE:SELF-VERIFY OBLIG:POLICY
   OBLIG-VERIFIER:DIFFERENTIAL-EXEC OBLIG:VERIFIER-CLASS
   ENV OBLIG:ENVIRONMENT  PROD OBLIG:PRODUCER  OBLIG:SEAL ;
\ Measure obligation: resource bound, performance domain, perf-benchmark, self-verify.
: OM ( -- OBLIG:obligation )
   OBLIG:NEW  MODEL OBLIG:SUBJECT
   OBLIG-RELATION:RESOURCE-BOUND OBLIG:RELATION
   OBLIG-DOMAIN:PERFORMANCE OBLIG:DOMAIN
   OBLIG-INDEPENDENCE:SELF-VERIFY OBLIG:POLICY
   OBLIG-VERIFIER:PERF-BENCHMARK OBLIG:VERIFIER-CLASS
   ENV OBLIG:ENVIRONMENT  PROD OBLIG:PRODUCER  OBLIG:SEAL ;

: EV ( -- OBLIG:evidence )           \ discharges OV
   MODEL OBLIG-DOMAIN:EXACT OBLIG-RELATION:SEMANTIC-EQUIV ENV VER OBLIG-VERIFIER:DIFFERENTIAL-EXEC OBLIG:EVIDENCE ;
: EM ( -- OBLIG:evidence )           \ discharges OM
   MODEL OBLIG-DOMAIN:PERFORMANCE OBLIG-RELATION:RESOURCE-BOUND ENV VER OBLIG-VERIFIER:PERF-BENCHMARK OBLIG:EVIDENCE ;
: EV-OTHER ( -- OBLIG:evidence )     \ wrong-target: about MODEL2, not OV's subject
   MODEL2 OBLIG-DOMAIN:EXACT OBLIG-RELATION:SEMANTIC-EQUIV ENV VER OBLIG-VERIFIER:DIFFERENTIAL-EXEC OBLIG:EVIDENCE ;
: EV-WRONGDOM ( -- OBLIG:evidence )  \ about MODEL but device domain: inapplicable for OV
   MODEL OBLIG-DOMAIN:DEVICE OBLIG-RELATION:SEMANTIC-EQUIV ENV VER OBLIG-VERIFIER:DIFFERENTIAL-EXEC OBLIG:EVIDENCE ;

\ ---- promotion policies --------------------------------------------------------
: SPEC-A ( -- PPOLICY:spec )   MODEL WTS TGT NUM POP VER 7 100 5000 RB PPOLICY:MK ;
: SPEC-B ( -- PPOLICY:spec )   MODEL WTS TGT NUM POP VER 7 200 5000 RB PPOLICY:MK ;  \ changed threshold
: SPEC-M2 ( -- PPOLICY:spec )  MODEL2 WTS TGT NUM POP VER 7 100 5000 RB PPOLICY:MK ; \ policy for wrong model

\ ---- session builders ----------------------------------------------------------
: SESSION-FULL ( -- )     \ OV+OM tracked, EV+EM available, no change -> both applicable
   PROMOTE:RESET
   OV PROMOTE:OBLIGATION+  OM PROMOTE:OBLIGATION+
   EV PROMOTE:EVIDENCE+    EM PROMOTE:EVIDENCE+ ;

\ MK-PROMOTED runs the whole chain over the full session; SPEC-A / now=1000 (< expiry 5000).
: MK-PROMOTED ( -- PROMOTE:promoted )
   SESSION-FULL
   MODEL PROMOTE:CANDIDATE
   OV PROMOTE:VERIFY
   OM PROMOTE:MEASURE
   SPEC-A 1000 PROMOTE:SATISFY
   PROMOTE:PROMOTE ;

\ ---- ACCEPTANCE 1: the TYPED reject (APPLIC:VERDICT) ---------------------------
: VORD ( OBLIG:obligation -- n )     \ 0 applicable / 1 stale / 2 missing / 3 inapplicable
   APPLIC:VERDICT MATCH APPLIC:applicability
      applicable   OF 0 ENDOF
      stale        OF 1 ENDOF
      missing      OF 2 ENDOF
      inapplicable OF 3 ENDOF
   ;MATCH ;
: PV-APPLICABLE ( -- n )
   PROMOTE:RESET OV PROMOTE:OBLIGATION+ EV PROMOTE:EVIDENCE+  OV VORD ;
: PV-MISSING ( -- n )                \ only wrong-target evidence present
   PROMOTE:RESET OV PROMOTE:OBLIGATION+ EV-OTHER PROMOTE:EVIDENCE+  OV VORD ;
: PV-STALE ( -- n )                  \ discharging evidence but the subject changed
   PROMOTE:RESET OV PROMOTE:OBLIGATION+ EV PROMOTE:EVIDENCE+ MODEL PROMOTE:CHANGE+  OV VORD ;
: PV-INAPPLICABLE ( -- n )           \ evidence about the subject but wrong domain
   PROMOTE:RESET OV PROMOTE:OBLIGATION+ EV-WRONGDOM PROMOTE:EVIDENCE+  OV VORD ;

\ ---- ACCEPTANCE 1: the CONSTRUCTOR refusal (transitions throw) -----------------
: VER-MISSING-CODE ( -- n )          \ VERIFY refuses when OV is not applicable
   PROMOTE:RESET OV PROMOTE:OBLIGATION+ EV-OTHER PROMOTE:EVIDENCE+
   [: MODEL PROMOTE:CANDIDATE OV PROMOTE:VERIFY PROMOTE:VER-MODEL drop ;] catch ;
: MEAS-MISSING-CODE ( -- n )         \ MEASURE refuses when OM is not applicable (EM absent)
   PROMOTE:RESET OV PROMOTE:OBLIGATION+ OM PROMOTE:OBLIGATION+ EV PROMOTE:EVIDENCE+
   [: MODEL PROMOTE:CANDIDATE OV PROMOTE:VERIFY OM PROMOTE:MEASURE PROMOTE:MEAS-MODEL drop ;] catch ;

\ ---- ACCEPTANCE 1: the STATIC leg (cad-kinds verdict pattern) ------------------
create VDIAG 4096 allot
: VCHECK ( ptr u8 n -- n )   VDIAG 4096 DIAG-BUFFER! CHECK-CANDIDATE! DIAG-BUFFER-OFF ;

\ ---- ACCEPTANCE 2: no mutation / identity threading ----------------------------
: ID-CAND ( -- bool )    MODEL PROMOTE:CANDIDATE PROMOTE:CAND-MODEL MODEL ARTIFACT:EQUAL? ;
: ID-THREAD ( -- bool )  MK-PROMOTED PROMOTE:PROM-MODEL MODEL ARTIFACT:EQUAL? ;

\ ---- ACCEPTANCE 3: policy change invalidates (digest-bound, both directions) ----
: REVAL-SAME ( -- bool )   MK-PROMOTED SPEC-A PROMOTE:REVALIDATE ;
: REVAL-DIFF ( -- bool )   MK-PROMOTED SPEC-B PROMOTE:REVALIDATE ;

\ ---- ACCEPTANCE 4: audit records the exact closure -----------------------------
: BYTES= ( ptr u8 n ptr u8 n -- bool ) {: a:ptr an:n b:ptr bn:n :}
   an bn <> if false exit then
   0 begin dup an < while
      dup {: k:n :}
      a k + c@  b k + c@  <> if drop false exit then
      1+
   repeat drop true ;
: AUDIT-MATCH ( -- bool )            \ recorded closure == recomputed closure
   MK-PROMOTED PROMOTE:REPLAY-DESC$  PROMOTE:CLOSURE-DESC$  BYTES= ;
: AUDIT-NONEMPTY ( -- bool )         \ the recorded closure is non-empty (2 obligations tracked)
   MK-PROMOTED PROMOTE:REPLAY-DESC$ nip 0 > ;

\ ---- SATISFY guards ------------------------------------------------------------
: SAT-MODEL-CODE ( -- n )            \ policy for a different model -> E-PROMO-MODEL
   SESSION-FULL
   [: MODEL PROMOTE:CANDIDATE OV PROMOTE:VERIFY OM PROMOTE:MEASURE
      SPEC-M2 1000 PROMOTE:SATISFY PROMOTE:PROMOTE PROMOTE:PROM-MODEL drop ;] catch ;
: SAT-EXPIRED-CODE ( -- n )          \ now >= expiry -> E-PROMO-EXPIRED
   SESSION-FULL
   [: MODEL PROMOTE:CANDIDATE OV PROMOTE:VERIFY OM PROMOTE:MEASURE
      SPEC-A 5000 PROMOTE:SATISFY PROMOTE:PROMOTE PROMOTE:PROM-MODEL drop ;] catch ;

T-RESET

\ ---- ACCEPTANCE 1: typed reject ------------------------------------------------
PV-APPLICABLE 0 T=
PV-STALE 1 T=
PV-MISSING 2 T=
PV-INAPPLICABLE 3 T=

\ ---- ACCEPTANCE 1: constructor refusal -----------------------------------------
VER-MISSING-CODE E-PROMO-UNAPPLICABLE T=
MEAS-MISSING-CODE E-PROMO-UNAPPLICABLE T=

\ ---- ACCEPTANCE 1: static unconstructibility -----------------------------------
\ positive control: a real Verified enters MEASURE and certifies
s" TSOK ( PROMOTE:verified OBLIG:obligation -- PROMOTE:measured ) PROMOTE:MEASURE" VCHECK -1 T=
\ a Candidate (wrong stage) cannot enter MEASURE
s" TSWS ( PROMOTE:candidate OBLIG:obligation -- PROMOTE:measured ) PROMOTE:MEASURE" VCHECK 0 T=
\ a raw n cannot stand where a Verified is required
s" TSRAW ( n OBLIG:obligation -- PROMOTE:measured ) PROMOTE:MEASURE" VCHECK 0 T=
\ a raw n cannot forge the sealed proof token slot of a Verified
s" TSTOK ( CAD-KIND:artifact-id n -- PROMOTE:verified ) PROMOTE-VERIFIED:MAKE" VCHECK 0 T=
\ the private stage mint does not resolve outside the package (unconstructible)
s" TSMINT ( CAD-KIND:artifact-id -- PROMOTE:verified ) PROMOTE:MINT-VER-PROOF PROMOTE-VERIFIED:MAKE" VCHECK 1 T=
s" PROMOTE:MINT-VER-PROOF" 0 search-wl 0= TTRUE
s" PROMOTE:MINT-PROM-PROOF" 0 search-wl 0= TTRUE

\ ---- ACCEPTANCE 2: no mutation / identity --------------------------------------
ID-CAND TTRUE
ID-THREAD TTRUE

\ ---- ACCEPTANCE 3: policy change invalidates -----------------------------------
REVAL-SAME TTRUE
REVAL-DIFF TFALSE

\ ---- ACCEPTANCE 4: audit exact closure -----------------------------------------
AUDIT-NONEMPTY TTRUE
AUDIT-MATCH TTRUE

\ ---- SATISFY guards ------------------------------------------------------------
SAT-MODEL-CODE E-PROMO-MODEL T=
SAT-EXPIRED-CODE E-PROMO-EXPIRED T=

T-REPORT

;package
