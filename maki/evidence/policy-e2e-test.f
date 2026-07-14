\ policy-e2e-test.f - executed end-to-end promotion policy acceptance (dot
\ habu-public-producers-for-7084d81c).
\
\ The acceptance the public producers + the TF-CTOR-NAME-LIMIT 16->32 raise unblocked:
\ POLICY:CHECK is RUN over a REAL EVID:bundle built from GENUINE values - artifact ids
\ from ARTIFACT:REGISTER, an ART:built from the public MODEL->...->ART:BUILD pipeline,
\ and evidence from the real EVID gate transitions - not pinned by a type candidate.
\ It proves the census probe-3 fact executably (MODEL-CAD-V2-PLAN.md:1576-1583):
\   - a matching, complete bundle GRANTS, and the grant is bound to that artifact;
\   - an absent required (blocking) slot refuses with E-EVID-MISSING;
\   - a present slot naming a DIFFERENT artifact refuses with E-EVID-ARTIFACT;
\   - ART:PROMOTE with a grant issued for a different artifact refuses (the tightening).
\ Every refusal is paired with a resolving positive control (per LESSONS), so no
\ TTHROWS is vacuous.

require lib/test.f
require test/checker-assert.f
require maki/target/target.f         \ TARGET:SM87 (the target-id the pipeline emits for)
require maki/artifact.f              \ ARTIFACT:REGISTER
require maki/evidence/promote.f      \ ART:PROMOTE (pulls policy.f -> schema.f -> typestate.f)

\ ---- build a real ART:built for an artifact through the public pipeline --------
: BUILT-FOR ( CAD-KIND:artifact-id -- ART:built )
   {: art:CAD-KIND:artifact-id :}
   MODEL:DECLARE MODEL:ELABORATE TIR:SOLVE RIR:LEGALIZE
   PLAN:DRAFT PLAN:FINISH KIR:DRAFT KIR:VERIFY   \ KIR:verified
   TARGET:SM87 CAND:EMIT                          \ CAND:emitted
   art ART:BUILD ;                                \ ART:built carrying art

\ ---- one genuine evidence value per class, each measured on the built artifact -
: CERT-FOR ( CAD-KIND:artifact-id -- EVID:certified )   BUILT-FOR EVID:CERTIFY ;
: GOLD-FOR ( CAD-KIND:artifact-id -- EVID:golden )
   BUILT-FOR EVID-GOLDEN--LEG:HOST EVID-PREC--CLASS:PREC-F32 NPOL-DOM:EXACT EVID:GOLDEN ;
: GRAD-FOR ( CAD-KIND:artifact-id -- EVID:gradchecked ) BUILT-FOR EVID:GRADCHECK ;
: PROF-FOR ( CAD-KIND:artifact-id -- EVID:profiled )    BUILT-FOR 100 EVID:PROFILE ;

\ ---- bundles: a complete one, and one with the (blocking) certify slot absent ---
: BUNDLE-FULL ( CAD-KIND:artifact-id -- EVID:bundle ) {: art:CAD-KIND:artifact-id :}
   art CERT-FOR EVID-CERTIFY--SLOT:CERTIFY-GOT
   art GOLD-FOR EVID-GOLDEN--SLOT:GOLDEN-GOT
   art GRAD-FOR EVID-GRADCHECK--SLOT:GRADCHECK-GOT
   art PROF-FOR EVID-PROFILE--SLOT:PROFILE-GOT
   EVID-BUNDLE:MAKE ;
: BUNDLE-NO-CERT ( CAD-KIND:artifact-id -- EVID:bundle ) {: art:CAD-KIND:artifact-id :}
   EVID-CERTIFY--SLOT:CERTIFY-NONE
   art GOLD-FOR EVID-GOLDEN--SLOT:GOLDEN-GOT
   art GRAD-FOR EVID-GRADCHECK--SLOT:GRADCHECK-GOT
   art PROF-FOR EVID-PROFILE--SLOT:PROFILE-GOT
   EVID-BUNDLE:MAKE ;

\ ---- run POLICY:CHECK against the default (V1) policy --------------------------
: GRANT-FOR ( EVID:bundle CAD-KIND:artifact-id -- POLICY:granted ) {: art:CAD-KIND:artifact-id :}
   art BUILT-FOR  POLICY:SCHEMA POLICY:DEFAULT-POLICY  POLICY:CHECK ;

\ green path: a complete matching bundle grants, and the grant names THAT artifact.
: E2E-GRANT-ART? ( -- bool )
   s" e2e-grant" ARTIFACT:REGISTER {: art:CAD-KIND:artifact-id :}
   art BUNDLE-FULL art GRANT-FOR
   POLICY-GRANTED:UNMAKE  drop drop      \ granted -> art (drop grant-proof + pol)
   art ARTIFACT:EQUAL? ;

\ red path: an absent required (blocking) certify slot refuses with E-EVID-MISSING.
: E2E-MISSING ( -- )
   s" e2e-miss" ARTIFACT:REGISTER {: art:CAD-KIND:artifact-id :}
   art BUNDLE-NO-CERT art GRANT-FOR
   POLICY-GRANTED:UNMAKE drop drop drop ;   \ unreached on refusal; balances the no-refusal case

\ red path: evidence naming artifact B, checked for artifact A, refuses E-EVID-ARTIFACT.
: E2E-WRONG-ART ( -- )
   s" e2e-a" ARTIFACT:REGISTER {: a:CAD-KIND:artifact-id :}
   s" e2e-b" ARTIFACT:REGISTER {: b:CAD-KIND:artifact-id :}
   b BUNDLE-FULL a GRANT-FOR
   POLICY-GRANTED:UNMAKE drop drop drop ;

\ promote tightening: a grant for A promotes A's built witness...
: E2E-PROMOTE-OK ( -- )
   s" e2e-p" ARTIFACT:REGISTER {: art:CAD-KIND:artifact-id :}
   art BUILT-FOR  art BUNDLE-FULL art GRANT-FOR  ART:PROMOTE drop ;
\ ...but a grant for A cannot promote B's built witness (E-EVID-ARTIFACT).
: E2E-PROMOTE-BAD ( -- )
   s" e2e-pa" ARTIFACT:REGISTER {: a:CAD-KIND:artifact-id :}
   s" e2e-pb" ARTIFACT:REGISTER {: b:CAD-KIND:artifact-id :}
   b BUILT-FOR  a BUNDLE-FULL a GRANT-FOR  ART:PROMOTE drop ;

\ ---- numeric-policy refusal: a golden's ACHIEVED domain rides in the record -----
\ A device golden judged under TF32 carries the relative-error proof domain
\ (EVID:GOLD-DOM). Against an exact-policy requirement it REFUSES (E-NPOL-APPROX,
\ maki/numpolicy.f NPOL:ENFORCE); against a relative requirement the SAME golden
\ satisfies - the "approximate evidence cannot satisfy an exact policy" boundary,
\ read off the real evidence record built through the pipeline.
: GOLD-REL-FOR ( -- EVID:golden )
   s" e2e-gnum" ARTIFACT:REGISTER BUILT-FOR
   EVID-GOLDEN--LEG:DEVICE EVID-PREC--CLASS:PREC-TF32 NPOL-DOM:RELATIVE EVID:GOLDEN ;
: E2E-GOLD-EXACT-REFUSE ( -- )  GOLD-REL-FOR EVID:GOLD-DOM  NPOL-DOM:EXACT     NPOL:ENFORCE ;
: E2E-GOLD-REL-OK      ( -- )   GOLD-REL-FOR EVID:GOLD-DOM  NPOL-DOM:RELATIVE  NPOL:ENFORCE ;

T-RESET

\ ---- executed end-to-end -----------------------------------------------------
E2E-GRANT-ART?                 T-ASSERT   \ green: grants, and the grant binds the artifact
' E2E-MISSING     E-EVID-MISSING  TTHROWS \ red: absent required slot
' E2E-WRONG-ART   E-EVID-ARTIFACT TTHROWS \ red: wrong-artifact evidence
' E2E-PROMOTE-OK  0               TTHROWS \ promote a matching (built, grant): no refusal
' E2E-PROMOTE-BAD E-EVID-ARTIFACT TTHROWS \ promote a mismatched (built, grant): refused
' E2E-GOLD-EXACT-REFUSE E-NPOL-APPROX TTHROWS \ relative golden vs exact policy: refused
' E2E-GOLD-REL-OK       0             TTHROWS \ relative golden vs relative policy: satisfied

T-REPORT
