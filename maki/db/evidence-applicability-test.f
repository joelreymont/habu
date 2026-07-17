\ maki/db/evidence-applicability-test.f - acceptance for the obligation closure +
\ evidence applicability checker (maki/db/evidence-applicability.f; dot
\ habu-v2-evidence-applicability-73ac58b9).
\
\ Proves the dot acceptance, each item by a named test word (all obligation / evidence
\ values are produced and consumed INSIDE colon words, never on the interpret stack):
\   MM-*     : the MUTATION MATRIX - from a matching (applicable) evidence, mutating EACH
\              single component (subject / domain / relation / environment / verifier-class /
\              verifier-identity) flips the verdict to exactly the affected result, and an
\              unrelated non-matching evidence in the pool does not break a matching one.
\   SVD-*    : "static proof cannot satisfy required device execution" - a static-checker
\              verdict is INAPPLICABLE to a device-measure obligation; a device-measure
\              verdict discharges it (the axis is exactly verifier-class).
\   PVE-*    : "performance evidence cannot satisfy equivalence" - a performance-domain
\              verdict is INAPPLICABLE to an exact semantic-equivalence obligation.
\   CL-*     : the CLOSURE - a change-set invalidates EXACTLY the affected obligations (a
\              per-slot bitmask), an unrelated change invalidates none, and the cache-hit
\              closure equals the uncached closure (CLOSURE-EQUAL?) for every change-set.

require lib/test.f
require maki/db/evidence-applicability.f
require maki/artifact.f
require maki/config.f
require maki/producer.f

package APPLIC-TEST

\ ---- shared identities (registered once; REGISTER interns by content) ----------
: SUBJ ( -- CAD-KIND:artifact-id )       s" applic-test/art-subject-1" ARTIFACT:REGISTER ;
: OTHER-SUBJ ( -- CAD-KIND:artifact-id ) s" applic-test/art-subject-2" ARTIFACT:REGISTER ;
: THIRD-SUBJ ( -- CAD-KIND:artifact-id ) s" applic-test/art-subject-3" ARTIFACT:REGISTER ;
: ENV ( -- CAD-KIND:config-id )          s" applic-test/env-sm87" CONFIG:REGISTER ;
: OTHER-ENV ( -- CAD-KIND:config-id )    s" applic-test/env-orin" CONFIG:REGISTER ;
: PROD ( -- CAD-KIND:producer-id )       s" applic-test/agent-search" PRODUCER:REGISTER ;
: VERIF ( -- CAD-KIND:producer-id )      s" applic-test/verifier-diff" PRODUCER:REGISTER ;
: DEPX ( -- CAD-KIND:artifact-id )       s" applic-test/art-dep-x" ARTIFACT:REGISTER ;
: DEPY ( -- CAD-KIND:artifact-id )       s" applic-test/art-dep-y" ARTIFACT:REGISTER ;
: DEPZ ( -- CAD-KIND:artifact-id )       s" applic-test/art-dep-z" ARTIFACT:REGISTER ;
: UNREL ( -- CAD-KIND:artifact-id )      s" applic-test/art-unrelated" ARTIFACT:REGISTER ;

\ ---- obligation fixtures (staged OBLIG builder) --------------------------------
\ Canonical: INDEPENDENT semantic-equivalence, exact domain, differential-exec verifier,
\ sm87 environment, subject art-subject-1, deps {dep-x, dep-y}, proposed by agent/search.
: OBL-CANON ( -- OBLIG:obligation )
   OBLIG:NEW
   SUBJ OBLIG:SUBJECT
   OBLIG-RELATION:SEMANTIC-EQUIV OBLIG:RELATION
   OBLIG-DOMAIN:EXACT OBLIG:DOMAIN
   OBLIG-INDEPENDENCE:INDEPENDENT OBLIG:POLICY
   OBLIG-VERIFIER:DIFFERENTIAL-EXEC OBLIG:VERIFIER-CLASS
   ENV OBLIG:ENVIRONMENT
   PROD OBLIG:PRODUCER
   DEPX OBLIG:DEP+  DEPY OBLIG:DEP+
   OBLIG:SEAL ;

\ Second obligation: subject art-subject-2, dep {dep-y} only.
: OBL-OTHER ( -- OBLIG:obligation )
   OBLIG:NEW
   OTHER-SUBJ OBLIG:SUBJECT
   OBLIG-RELATION:SEMANTIC-EQUIV OBLIG:RELATION
   OBLIG-DOMAIN:EXACT OBLIG:DOMAIN
   OBLIG-INDEPENDENCE:INDEPENDENT OBLIG:POLICY
   OBLIG-VERIFIER:DIFFERENTIAL-EXEC OBLIG:VERIFIER-CLASS
   ENV OBLIG:ENVIRONMENT
   PROD OBLIG:PRODUCER
   DEPY OBLIG:DEP+
   OBLIG:SEAL ;

\ Third obligation: subject art-subject-3, dep {dep-z} only (disjoint from the others).
: OBL-Z ( -- OBLIG:obligation )
   OBLIG:NEW
   THIRD-SUBJ OBLIG:SUBJECT
   OBLIG-RELATION:SEMANTIC-EQUIV OBLIG:RELATION
   OBLIG-DOMAIN:EXACT OBLIG:DOMAIN
   OBLIG-INDEPENDENCE:INDEPENDENT OBLIG:POLICY
   OBLIG-VERIFIER:DIFFERENTIAL-EXEC OBLIG:VERIFIER-CLASS
   ENV OBLIG:ENVIRONMENT
   PROD OBLIG:PRODUCER
   DEPZ OBLIG:DEP+
   OBLIG:SEAL ;

\ Obligation requiring a DEVICE-MEASURE verifier (subject art-subject-1, dep {dep-x}).
: OBL-DEVICE-REQ ( -- OBLIG:obligation )
   OBLIG:NEW
   SUBJ OBLIG:SUBJECT
   OBLIG-RELATION:SEMANTIC-EQUIV OBLIG:RELATION
   OBLIG-DOMAIN:EXACT OBLIG:DOMAIN
   OBLIG-INDEPENDENCE:INDEPENDENT OBLIG:POLICY
   OBLIG-VERIFIER:DEVICE-MEASURE OBLIG:VERIFIER-CLASS
   ENV OBLIG:ENVIRONMENT
   PROD OBLIG:PRODUCER
   DEPX OBLIG:DEP+
   OBLIG:SEAL ;

\ ---- evidence fixtures (subject domain relation environment verifier verifier-class) ---
: EV-MATCH ( -- OBLIG:evidence )
   SUBJ OBLIG-DOMAIN:EXACT OBLIG-RELATION:SEMANTIC-EQUIV ENV VERIF OBLIG-VERIFIER:DIFFERENTIAL-EXEC OBLIG:EVIDENCE ;
: EV-WRONG-SUBJ ( -- OBLIG:evidence )
   OTHER-SUBJ OBLIG-DOMAIN:EXACT OBLIG-RELATION:SEMANTIC-EQUIV ENV VERIF OBLIG-VERIFIER:DIFFERENTIAL-EXEC OBLIG:EVIDENCE ;
: EV-DEVICE ( -- OBLIG:evidence )              \ wrong DOMAIN (a device measurement)
   SUBJ OBLIG-DOMAIN:DEVICE OBLIG-RELATION:SEMANTIC-EQUIV ENV VERIF OBLIG-VERIFIER:DIFFERENTIAL-EXEC OBLIG:EVIDENCE ;
: EV-PERF ( -- OBLIG:evidence )                \ a performance measurement (wrong domain + verifier)
   SUBJ OBLIG-DOMAIN:PERFORMANCE OBLIG-RELATION:SEMANTIC-EQUIV ENV VERIF OBLIG-VERIFIER:PERF-BENCHMARK OBLIG:EVIDENCE ;
: EV-WRONG-REL ( -- OBLIG:evidence )
   SUBJ OBLIG-DOMAIN:EXACT OBLIG-RELATION:RESOURCE-BOUND ENV VERIF OBLIG-VERIFIER:DIFFERENTIAL-EXEC OBLIG:EVIDENCE ;
: EV-WRONG-ENV ( -- OBLIG:evidence )
   SUBJ OBLIG-DOMAIN:EXACT OBLIG-RELATION:SEMANTIC-EQUIV OTHER-ENV VERIF OBLIG-VERIFIER:DIFFERENTIAL-EXEC OBLIG:EVIDENCE ;
: EV-STATIC ( -- OBLIG:evidence )              \ a static-checker verdict
   SUBJ OBLIG-DOMAIN:EXACT OBLIG-RELATION:SEMANTIC-EQUIV ENV VERIF OBLIG-VERIFIER:STATIC-CHECKER OBLIG:EVIDENCE ;
: EV-DEVICE-MEASURE ( -- OBLIG:evidence )      \ a device-measure verdict
   SUBJ OBLIG-DOMAIN:EXACT OBLIG-RELATION:SEMANTIC-EQUIV ENV VERIF OBLIG-VERIFIER:DEVICE-MEASURE OBLIG:EVIDENCE ;
: EV-SELF ( -- OBLIG:evidence )                \ verifier IS the producer (agent/search)
   SUBJ OBLIG-DOMAIN:EXACT OBLIG-RELATION:SEMANTIC-EQUIV ENV PROD OBLIG-VERIFIER:DIFFERENTIAL-EXEC OBLIG:EVIDENCE ;

\ ---- verdict inspector: 0 applicable / 1 stale / 2 missing / 3 inapplicable -----
: VCODE ( APPLIC:applicability -- n )
   MATCH APPLIC:applicability
      applicable   OF 0 ENDOF
      stale        OF 1 ENDOF
      missing      OF 2 ENDOF
      inapplicable OF 3 ENDOF
   ;MATCH ;

\ ---- mutation matrix: one obligation, one evidence, no changes -----------------
: MM-VERDICT ( OBLIG:evidence -- n ) {: e:OBLIG:evidence :}
   APPLIC:RESET
   e APPLIC:EVIDENCE+
   OBL-CANON APPLIC:VERDICT VCODE ;

: MM-MATCH ( -- n )       EV-MATCH MM-VERDICT ;          \ applicable
: MM-SUBJ ( -- n )        EV-WRONG-SUBJ MM-VERDICT ;     \ missing (evidence for another subject)
: MM-DOMAIN ( -- n )      EV-DEVICE MM-VERDICT ;         \ inapplicable (wrong domain)
: MM-RELATION ( -- n )    EV-WRONG-REL MM-VERDICT ;      \ inapplicable (wrong relation)
: MM-ENV ( -- n )         EV-WRONG-ENV MM-VERDICT ;      \ inapplicable (wrong environment)
: MM-VC ( -- n )          EV-STATIC MM-VERDICT ;         \ inapplicable (wrong verifier-class)
: MM-INDEP ( -- n )       EV-SELF MM-VERDICT ;           \ inapplicable (producer == verifier under independent)

\ A non-matching evidence in the pool does NOT break a matching one (ANY discharges).
: MM-POOL ( -- n )
   APPLIC:RESET
   EV-DEVICE APPLIC:EVIDENCE+
   EV-MATCH APPLIC:EVIDENCE+
   OBL-CANON APPLIC:VERDICT VCODE ;

\ ---- static proof cannot satisfy required device execution ---------------------
: SVD-STATIC ( -- n )                            \ inapplicable: static-checker vs device-measure
   APPLIC:RESET  EV-STATIC APPLIC:EVIDENCE+  OBL-DEVICE-REQ APPLIC:VERDICT VCODE ;
: SVD-DEVICE ( -- n )                            \ applicable positive control: device-measure discharges
   APPLIC:RESET  EV-DEVICE-MEASURE APPLIC:EVIDENCE+  OBL-DEVICE-REQ APPLIC:VERDICT VCODE ;

\ ---- performance evidence cannot satisfy equivalence ---------------------------
: PVE-PERF ( -- n )                              \ inapplicable: performance vs exact semantic-equiv
   EV-PERF MM-VERDICT ;

\ ---- closure: track three obligations (slots 0,1,2) then apply a change-set -----
\ slot 0 = OBL-CANON (subject SUBJ, deps {x,y}); slot 1 = OBL-OTHER (subject OTHER-SUBJ,
\ dep {y}); slot 2 = OBL-Z (subject THIRD-SUBJ, dep {z}).
: SETUP-CLOSURE ( -- )
   APPLIC:RESET
   OBL-CANON APPLIC:OBLIGATION+
   OBL-OTHER APPLIC:OBLIGATION+
   OBL-Z APPLIC:OBLIGATION+ ;

: CL-X ( -- )    SETUP-CLOSURE  DEPX APPLIC:CHANGE+ ;               \ touches slot 0 only
: CL-Y ( -- )    SETUP-CLOSURE  DEPY APPLIC:CHANGE+ ;               \ touches slots 0 + 1
: CL-SUBJ ( -- ) SETUP-CLOSURE  SUBJ APPLIC:CHANGE+ ;              \ touches slot 0 (subject)
: CL-OSUBJ ( -- ) SETUP-CLOSURE OTHER-SUBJ APPLIC:CHANGE+ ;        \ touches slot 1 (subject)
: CL-ZSUBJ ( -- ) SETUP-CLOSURE THIRD-SUBJ APPLIC:CHANGE+ ;        \ touches slot 2 (subject)
: CL-UNREL ( -- ) SETUP-CLOSURE UNREL APPLIC:CHANGE+ ;             \ touches nothing
: CL-XZ ( -- )   SETUP-CLOSURE  DEPX APPLIC:CHANGE+  DEPZ APPLIC:CHANGE+ ;  \ slots 0 + 2

\ uncached bitmask after a change-set builder runs
: U ( -- n )   APPLIC:INVALIDATED-SET-UNCACHED ;
: EQ ( -- bool )   APPLIC:CLOSURE-EQUAL? ;

T-RESET

\ ---- MUTATION MATRIX -----------------------------------------------------------
MM-MATCH 0 T=                          \ matching evidence -> applicable
MM-SUBJ 2 T=                           \ subject mismatch -> missing
MM-DOMAIN 3 T=                         \ domain mismatch -> inapplicable
MM-RELATION 3 T=                       \ relation mismatch -> inapplicable
MM-ENV 3 T=                            \ environment mismatch -> inapplicable
MM-VC 3 T=                             \ verifier-class mismatch -> inapplicable
MM-INDEP 3 T=                          \ independence violation -> inapplicable
MM-POOL 0 T=                           \ a non-matching evidence does not break a matching one

\ ---- static proof cannot satisfy required device execution ---------------------
SVD-STATIC 3 T=                        \ static-checker cannot discharge a device-measure obligation
SVD-DEVICE 0 T=                        \ a device measurement does (the axis is verifier-class)

\ ---- performance evidence cannot satisfy equivalence ---------------------------
PVE-PERF 3 T=                          \ performance evidence -> inapplicable to a semantic-equiv obligation

\ ---- CLOSURE: minimal invalidation set + cache equals uncached -----------------
CL-X    U 1 T=   EQ TTRUE              \ dep-x change -> exactly slot 0 (bit 0)
CL-Y    U 3 T=   EQ TTRUE              \ dep-y change -> slots 0 + 1 (bits 0,1)
CL-SUBJ U 1 T=   EQ TTRUE              \ subject change -> slot 0
CL-OSUBJ U 2 T=  EQ TTRUE              \ other subject change -> slot 1 only
CL-ZSUBJ U 4 T=  EQ TTRUE              \ third subject change -> slot 2 only
CL-UNREL U 0 T=  EQ TTRUE              \ an unrelated change invalidates nothing
CL-XZ   U 5 T=   EQ TTRUE              \ deps x + z -> slots 0 + 2 (bits 0,2)

T-REPORT

;package
