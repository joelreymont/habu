\ maki/db/diff-case-store-xproc-child.f - the FRESH-PROCESS decode side of the decisive
\ cross-process durability test for the per-case output store (maki/db/diff-case-store.f;
\ dot habu-v2-differential-runner-13359019).
\
\ CONCERN: the shared suite/environment/subject + deterministic per-case outcomes used by
\ BOTH the parent (STORE-ALL: run cases, PUT durable records into a shared store dir) and the
\ FRESH child (VERIFY-ALL: point the store at the same dir, rebuild each case, LOAD, and
\ byte-match the rehydrated record against a re-derived expected record). Package CSXP.
\
\ The point it proves: a durable record written in one process rehydrates and byte-matches in
\ a FRESH bin/hb whose identity registry is deliberately SHIFTED (decoys registered first).
\ The store key composes cross-process CONTENT keys (subject / suite-digest / environment) plus
\ the deterministic case-id, all registration-order-independent, so the rebuilt suite/environment/
\ subject derives byte-identical store keys and finds the records - durable identity survives
\ process death (the maki/db/keywire-xproc content-key property, applied to the whole record).

require lib/prelude.f
require lib/string.f
require lib/fs.f
require maki/db/diff-case-store.f
require maki/db/diff-runner.f
require maki/db/diff-suite.f
require maki/numpolicy.f
require maki/producer.f
require maki/config.f
require maki/target/target.f
require maki/db/obligation.f
require maki/db/budget-dim.f

package CSXP
public

6 constant NCASES                  \ cases [0,NCASES); case 2 faults, case 4 skips reference

private

create EREC 256 allot              \ re-derived expected record (byte-match)
variable OKF                        \ all-cases-verified flag

: MEM= ( ptr u8 ptr u8 n -- bool ) {: pa:ptr pb:ptr n:n :}
   0 begin dup n < while
      dup {: k:n :}
      pa k + c@  pb k + c@  <> if drop false exit then
      1+
   repeat drop true ;

: FAIL! ( -- )   false OKF ! ;

\ ---- readable wrappers over the DIFFRUN result constructors ---------------------
: >PROD ( n -- DIFFRUN:run-result )   DIFFRUN-RUN--RESULT:PRODUCED ;
: >FLT ( -- DIFFRUN:run-result )      DIFFRUN-RUN--RESULT:FAULTED ;
: >VAL ( n -- DIFFRUN:ref-result )    DIFFRUN-REF--RESULT:VALUE ;
: >SKP ( -- DIFFRUN:ref-result )      DIFFRUN-REF--RESULT:SKIP ;
: >AGR ( -- DIFFRUN:case-verdict )    DIFFRUN-CASE--VERDICT:AGREE ;
: >SFLT ( -- DIFFRUN:case-verdict )   DIFFRUN-CASE--VERDICT:SUBJECT-FAULT ;
: >RSKP ( -- DIFFRUN:case-verdict )   DIFFRUN-CASE--VERDICT:REFERENCE-SKIP ;

\ ---- shared identities (content-addressed; identical in parent and child) -------
: SUBJ ( -- CAD-KIND:producer-id )  s" casestore-xproc/subject" PRODUCER:REGISTER ;
: CMP-EXACT ( -- CAD-KIND:numeric-policy-id )  NPOL-DOM:EXACT NPOL:REGISTER ;

public
: ENV ( -- CAD-KIND:config-id )   s" casestore-xproc/env" CONFIG:REGISTER ;

\ BUILD-SUITE seals the shared scalar suite (identical fields -> identical digest -> identical
\ store keys in both processes). Same shape as the diff-runner-test scalar fixture.
: BUILD-SUITE ( -- DIFFSUITE:suite )
   DIFFSUITE:NEW
   42 DIFFSUITE:SEED!
   SUBJ DIFFSUITE:SUBJECT
   OBLIG-INDEPENDENCE:SELF-VERIFY DIFFSUITE:POLICY
   CMP-EXACT 0 DIFFSUITE:COMPARISON
   s" casestore-xproc/norm" DIFFSUITE:NORMALIZATION
   s" casestore-xproc/min" DIFFSUITE:MINIMIZER
   TARGET:SM87 DIFFSUITE:TARGET-NEED
   BUDGET-DIM:COMPUTE-TIME 100 DIFFSUITE:BUDGET!
   s" casestore-xproc/gen" DIFFSUITE:GENERATOR+
   SUBJ DIFFSUITE:REFERENCE+
   s" casestore-xproc/prop" DIFFSUITE:PROPERTY+
   DIFFSUITE:SEAL MATCH DIFFSUITE:build-result
      ok OF ENDOF
      incomplete OF -779 throw ENDOF
      tolerance-mismatch OF -779 throw ENDOF
      reference-not-independent OF -779 throw ENDOF
   ;MATCH ;

private

\ ---- deterministic per-case outcomes (shared by STORE-ALL and VERIFY-ALL) -------
\ subject(k)=produced k, except case 2 faults; reference(k)=value k, except case 4 skips;
\ verdict(k)=subject-fault / reference-skip / agree (subject k == reference k otherwise).
: C-SUBJ ( n -- DIFFRUN:run-result )    dup 2 = if drop >FLT else >PROD then ;
: C-REF ( n -- DIFFRUN:ref-result )     dup 4 = if drop >SKP else >VAL then ;
: C-VERD ( n -- DIFFRUN:case-verdict )
   dup 2 = if drop >SFLT exit then
   dup 4 = if drop >RSKP exit then
   drop >AGR ;

\ SHIFT registers decoys FIRST so the child's real subject/environment get raw registry indices
\ that DIFFER from the parent's - the wedge that proves the store key is content-derived, not
\ registration-order-derived.
: SHIFT ( -- )
   s" casestore-decoy-1" PRODUCER:REGISTER drop
   s" casestore-decoy-2" PRODUCER:REGISTER drop
   s" casestore-dc-1" CONFIG:REGISTER drop ;

: VERIFY-CASE ( DIFFSUITE:suite CAD-KIND:config-id n -- ) {: h:DIFFSUITE:suite env:CAD-KIND:config-id k:n :}
   h env k  k C-SUBJ  k C-REF  k C-VERD  EREC CASESTORE:RECORD-INTO   \ expected bytes
   h env k CASESTORE:LOAD MATCH CASESTORE:load-result
      ok OF {: s:n :}
         s CASESTORE:REC@ drop  EREC  CASESTORE:REC-WIDTH  MEM= 0= if FAIL! then ENDOF
      absent    OF FAIL! ENDOF
      malformed OF FAIL! ENDOF
      mismatch  OF FAIL! ENDOF
   ;MATCH ;

public

\ STORE-ALL (parent side): run every case and PUT its durable record into the store.
: STORE-ALL ( -- )
   BUILD-SUITE {: h:DIFFSUITE:suite :}
   ENV {: env:CAD-KIND:config-id :}
   0 begin dup NCASES < while
      dup {: k:n :}
      h env k  k C-SUBJ  k C-REF  k C-VERD  CASESTORE:PUT
      1+
   repeat drop ;

\ VERIFY-ALL (fresh-child side): point the store at the parent's dir, SHIFT the registry, rebuild
\ each case, LOAD, and byte-match. Prints CSXP-OK iff every rehydrated record byte-matches the
\ re-derived expected record; else CSXP-FAIL. The printed token is the parent's captured verdict.
: VERIFY-ALL ( ptr u8 n -- ) {: p:ptr u:n :}
   p u CASESTORE:ROOT!
   true OKF !
   SHIFT
   BUILD-SUITE {: h:DIFFSUITE:suite :}
   ENV {: env:CAD-KIND:config-id :}
   0 begin dup NCASES < while
      dup {: k:n :}
      h env k VERIFY-CASE
      1+
   repeat drop
   OKF @ if s" CSXP-OK" else s" CSXP-FAIL" then type ;

;package
