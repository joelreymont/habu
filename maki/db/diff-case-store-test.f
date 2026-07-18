\ maki/db/diff-case-store-test.f - in-process acceptance for the durable per-case output
\ store (maki/db/diff-case-store.f; dot habu-v2-differential-runner-13359019).
\
\ Proves, against a REAL private file store, that every executed case's input identity,
\ subject + reference OUTPUTS, classified OUTCOME, and environment digest are durably stored
\ and rehydrated - and that the store key composes subject || suite-digest || environment ||
\ case-id (flipping the environment or the case yields a distinct durable record). Each public
\ word is exercised. The decisive cross-PROCESS byte-match (encode here, decode in a fresh
\ bin/hb) is maki/db/diff-case-store-xproc-test.f.
\
\ Result sums / products are built and consumed INSIDE colon words (never on the interpret
\ stack). Identity fixtures mint real ids through owner constructors (never a raw cast) and
\ carry the casestore-test prefix (docs/forth.md "fixtures use unique test-owned names").

require lib/test.f
require lib/string.f
require lib/fs.f
require lib/fs-mutate.f
require maki/db/diff-case-store.f
require maki/db/diff-runner.f
require maki/db/diff-suite.f
require maki/numpolicy.f
require maki/producer.f
require maki/config.f
require maki/target/target.f
require maki/db/obligation.f
require maki/db/budget-dim.f

package CASESTORE-TEST

create EXPREC 256 allot           \ RECORD-INTO byte-match buffer
create BADREC 256 allot           \ wrong-content corruption buffer

\ One private store dir for the whole suite; tests RESET their files as needed.
s" hb-casestore-test" TMPDIR-MKDIR CASESTORE:ROOT!

: MEM= ( ptr u8 ptr u8 n -- bool ) {: pa:ptr pb:ptr n:n :}
   0 begin dup n < while
      dup {: k:n :}
      pa k + c@  pb k + c@  <> if drop false exit then
      1+
   repeat drop true ;

: ZERO-FILL ( ptr u8 n -- ) {: a:ptr n:n :}
   0 begin dup n < while
      dup {: k:n :}
      0 a k + c!
      1+
   repeat drop ;

\ ---- readable wrappers over the DIFFRUN result constructors --------------------------
: >PROD ( n -- DIFFRUN:run-result )   DIFFRUN-RUN--RESULT:PRODUCED ;
: >FLT ( -- DIFFRUN:run-result )      DIFFRUN-RUN--RESULT:FAULTED ;
: >VAL ( n -- DIFFRUN:ref-result )    DIFFRUN-REF--RESULT:VALUE ;
: >SKP ( -- DIFFRUN:ref-result )      DIFFRUN-REF--RESULT:SKIP ;
: >AGR ( -- DIFFRUN:case-verdict )    DIFFRUN-CASE--VERDICT:AGREE ;
: >MIS ( -- DIFFRUN:case-verdict )    DIFFRUN-CASE--VERDICT:MISMATCH ;
: >SFLT ( -- DIFFRUN:case-verdict )   DIFFRUN-CASE--VERDICT:SUBJECT-FAULT ;

\ ---- shared identities (REGISTER interns by content) --------------------------------
: SUBJ-A ( -- CAD-KIND:producer-id )  s" casestore-test/subject-a" PRODUCER:REGISTER ;
: SUBJ-B ( -- CAD-KIND:producer-id )  s" casestore-test/subject-b" PRODUCER:REGISTER ;
: CMP-EXACT ( -- CAD-KIND:numeric-policy-id )  NPOL-DOM:EXACT NPOL:REGISTER ;
: ENV-A ( -- CAD-KIND:config-id )  s" casestore-test/env-a" CONFIG:REGISTER ;
: ENV-B ( -- CAD-KIND:config-id )  s" casestore-test/env-b" CONFIG:REGISTER ;

\ ---- scalar suite fixture (exact domain, zero tolerance) - the diff-runner-test shape --
: BUILD-SCALAR ( CAD-KIND:producer-id n -- DIFFSUITE:suite )
   {: subj:CAD-KIND:producer-id seed:n :}
   DIFFSUITE:NEW
   seed DIFFSUITE:SEED!
   subj DIFFSUITE:SUBJECT
   OBLIG-INDEPENDENCE:SELF-VERIFY DIFFSUITE:POLICY
   CMP-EXACT 0 DIFFSUITE:COMPARISON
   s" casestore-test/norm" DIFFSUITE:NORMALIZATION
   s" casestore-test/min" DIFFSUITE:MINIMIZER
   TARGET:SM87 DIFFSUITE:TARGET-NEED
   BUDGET-DIM:COMPUTE-TIME 100 DIFFSUITE:BUDGET!
   s" casestore-test/gen" DIFFSUITE:GENERATOR+
   subj DIFFSUITE:REFERENCE+
   s" casestore-test/prop" DIFFSUITE:PROPERTY+
   DIFFSUITE:SEAL MATCH DIFFSUITE:build-result
      ok OF ENDOF
      incomplete OF -777 throw ENDOF
      tolerance-mismatch OF -777 throw ENDOF
      reference-not-independent OF -777 throw ENDOF
   ;MATCH ;

: SUITE-A ( -- DIFFSUITE:suite )   SUBJ-A 42 BUILD-SCALAR ;

\ ---- load-result projections --------------------------------------------------------
: LR>N ( CASESTORE:load-result<n> -- n )   \ 0 ok / 1 absent / 2 malformed / 3 mismatch
   MATCH CASESTORE:load-result
      ok        OF drop 0 ENDOF
      absent    OF 1 ENDOF
      malformed OF 2 ENDOF
      mismatch  OF 3 ENDOF
   ;MATCH ;

: LOAD-SLOT ( DIFFSUITE:suite CAD-KIND:config-id n -- n )   \ ok -> slot, else -777 throw
   CASESTORE:LOAD MATCH CASESTORE:load-result
      ok        OF ENDOF
      absent    OF -777 throw ENDOF
      malformed OF -777 throw ENDOF
      mismatch  OF -777 throw ENDOF
   ;MATCH ;

\ ---- durable round-trip: store a produced/value/agree case, rehydrate every field ----
: T-ROUNDTRIP ( -- bool )
   CASESTORE:RESET
   SUITE-A ENV-A 3  42 >PROD  42 >VAL  >AGR  CASESTORE:PUT
   SUITE-A ENV-A 3 LOAD-SLOT {: s:n :}
   s CASESTORE:PARAM@ 3 =
   s CASESTORE:SUBJ-KIND@ 0 = and
   s CASESTORE:SUBJ-VAL@ 42 = and
   s CASESTORE:REF-KIND@ 0 = and
   s CASESTORE:REF-VAL@ 42 = and
   s CASESTORE:VERDICT@ 0 = and ;

\ ---- fault/skip outcome lowers to its byte-stable kinds ------------------------------
: T-FAULT ( -- bool )
   CASESTORE:RESET
   SUITE-A ENV-A 4  >FLT  >SKP  >SFLT  CASESTORE:PUT
   SUITE-A ENV-A 4 LOAD-SLOT {: s:n :}
   s CASESTORE:SUBJ-KIND@ 1 =
   s CASESTORE:SUBJ-VAL@ 0 = and
   s CASESTORE:REF-KIND@ 1 = and
   s CASESTORE:REF-VAL@ 0 = and
   s CASESTORE:VERDICT@ 2 = and ;

\ ---- HAS? / absent -------------------------------------------------------------------
: T-HAS-YES ( -- bool )
   CASESTORE:RESET
   SUITE-A ENV-A 3  7 >PROD  7 >VAL  >AGR  CASESTORE:PUT
   SUITE-A ENV-A 3 CASESTORE:HAS? ;
: T-HAS-NO ( -- bool )
   CASESTORE:RESET  SUITE-A ENV-A 99 CASESTORE:HAS? 0= ;
: T-ABSENT ( -- n )
   CASESTORE:RESET  SUITE-A ENV-A 99 CASESTORE:LOAD LR>N ;

\ ---- RECORD-INTO single-sources the exact bytes a LOAD rehydrates --------------------
: T-RECORD-MATCH ( -- bool )
   CASESTORE:RESET
   SUITE-A ENV-A 5  9 >PROD  9 >VAL  >AGR  EXPREC CASESTORE:RECORD-INTO
   SUITE-A ENV-A 5  9 >PROD  9 >VAL  >AGR  CASESTORE:PUT
   SUITE-A ENV-A 5 LOAD-SLOT {: s:n :}
   s CASESTORE:REC@ drop  EXPREC  CASESTORE:REC-WIDTH  MEM= ;   \ drop REC@'s length (== REC-WIDTH)

\ ---- environment-keyed: same suite+case under two environments do not collide --------
: T-ENV-KEYED ( -- bool )
   CASESTORE:RESET
   SUITE-A ENV-A 3  42 >PROD  42 >VAL  >AGR  CASESTORE:PUT
   SUITE-A ENV-B 3  99 >PROD  99 >VAL  >AGR  CASESTORE:PUT
   SUITE-A ENV-A 3 LOAD-SLOT {: sa:n :}
   SUITE-A ENV-B 3 LOAD-SLOT {: sb:n :}
   sa CASESTORE:SUBJ-VAL@ 42 =
   sb CASESTORE:SUBJ-VAL@ 99 = and ;

\ ---- subject-keyed: a different subject (suite digest flips) -> a distinct record -----
: T-SUBJECT-KEYED ( -- bool )
   CASESTORE:RESET
   SUBJ-A 42 BUILD-SCALAR ENV-A 3  1 >PROD  1 >VAL  >AGR  CASESTORE:PUT
   SUBJ-B 42 BUILD-SCALAR ENV-A 3  2 >PROD  2 >VAL  >AGR  CASESTORE:PUT
   SUBJ-A 42 BUILD-SCALAR ENV-A 3 LOAD-SLOT {: sa:n :}
   SUBJ-B 42 BUILD-SCALAR ENV-A 3 LOAD-SLOT {: sb:n :}
   sa CASESTORE:SUBJ-VAL@ 1 =
   sb CASESTORE:SUBJ-VAL@ 2 = and ;

\ ---- distinct case ids -> distinct durable records -----------------------------------
: T-CASE-KEYED ( -- bool )
   CASESTORE:RESET
   SUITE-A ENV-A 3  30 >PROD  30 >VAL  >AGR  CASESTORE:PUT
   SUITE-A ENV-A 7  70 >PROD  70 >VAL  >AGR  CASESTORE:PUT
   SUITE-A ENV-A 3 LOAD-SLOT {: s3:n :}
   SUITE-A ENV-A 7 LOAD-SLOT {: s7:n :}
   s3 CASESTORE:PARAM@ 3 =
   s7 CASESTORE:PARAM@ 7 = and ;

\ ---- PATH$ is the content-addressed path; corruption is caught fail-closed -----------
: T-PATH-STABLE ( -- bool )   \ same identity -> same content-addressed path across calls
   CASESTORE:RESET
   SUITE-A ENV-A 3 CASESTORE:PATH$ {: pa:ptr pu:n :}
   pa EXPREC pu BYTE-COPY                        \ copy the first spelling (PATH-BUF is reused)
   SUITE-A ENV-A 3 CASESTORE:PATH$ {: pb:ptr pu2:n :}
   pu pu2 = if EXPREC pb pu MEM= else false then ;

: T-MALFORMED ( -- n )        \ a present file of the wrong size -> malformed
   CASESTORE:RESET
   SUITE-A ENV-A 3  5 >PROD  5 >VAL  >AGR  CASESTORE:PUT
   SUITE-A ENV-A 3 CASESTORE:PATH$ s" xx" WRITE-ALL
   SUITE-A ENV-A 3 CASESTORE:LOAD LR>N ;

: T-MISMATCH ( -- n )         \ a present REC-W file whose descriptor does not match -> mismatch
   CASESTORE:RESET
   SUITE-A ENV-A 3  5 >PROD  5 >VAL  >AGR  CASESTORE:PUT
   BADREC CASESTORE:REC-WIDTH ZERO-FILL
   SUITE-A ENV-A 3 CASESTORE:PATH$ BADREC CASESTORE:REC-WIDTH WRITE-ALL
   SUITE-A ENV-A 3 CASESTORE:LOAD LR>N ;

\ ---- ROOT+ creates the store tree ----------------------------------------------------
: T-ROOT-PLUS ( -- bool )   CASESTORE:ROOT+ EXISTS? ;

T-RESET

T-ROUNDTRIP TTRUE          \ input/outputs/outcome/environment all durably rehydrated
T-FAULT TTRUE              \ fault + skip lower to their byte-stable kinds
T-HAS-YES TTRUE
T-HAS-NO TTRUE
T-ABSENT 1 T=             \ no durable record -> absent
T-RECORD-MATCH TTRUE       \ RECORD-INTO bytes == the rehydrated REC@ bytes
T-ENV-KEYED TTRUE          \ environment is part of the store key
T-SUBJECT-KEYED TTRUE      \ subject (via suite digest) is part of the store key
T-CASE-KEYED TTRUE         \ the per-case id distinguishes durable records
T-PATH-STABLE TTRUE
T-MALFORMED 2 T=          \ wrong-size file -> malformed
T-MISMATCH 3 T=          \ wrong-content file at the content path -> mismatch
T-ROOT-PLUS TTRUE

T-REPORT

;package
