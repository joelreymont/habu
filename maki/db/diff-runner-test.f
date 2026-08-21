\ maki/db/diff-runner-test.f - acceptance for the differential runner CORE
\ (maki/db/diff-runner.f; dot habu-v2-differential-runner-13359019).
\
\ Drives the runner over a SCALAR CHECKER suite (exact domain, integer scalars) with
\ deterministic IN-PROCESS scripted adapters, proving the dot's acceptance (all sum /
\ product values produced + consumed INSIDE colon words, never on the interpret-mode
\ stack, per diff-suite-test):
\   A-* : ACCEPTANCE (a) - an injected mismatch MINIMIZES to its minimal counterexample and
\         REPLAYS deterministically; the minimizer is a pure function of (suite,p) (same both
\         runs), the minimized case still mismatches, and it is a SEPARATE content-addressed
\         artifact (CASE-ID(p') != CASE-ID(p)) - the original failing case is untouched.
\   B-* : ACCEPTANCE (b) - a hung/dying subject reaps as its OWN taxonomy member, never a
\         wrong value: CLASSIFY-OUTCOME maps timeout/signal/nonzero-exit to fault and only
\         exit(0) to produced-eligible; a scripted fault at an otherwise-agreeing case grades
\         subject-fault (2), DISTINCT from a genuine mismatch (1). The REAL spawn-isolated
\         proof (an actual bin/hb child that hangs / dies) is maki/db/diff-runner-spawn-test.f.
\   C-* : ACCEPTANCE (c) - a reference legs SKIP is recorded (reference-skip), never a
\         mismatch, when the reference is unavailable (the off-device / no-PyTorch case). The
\         scripted fake reference is the deterministic in-gate reference; the external-process
\         PyTorch adapter's off-device skip is proven in the spawn test.
\   D-* : ACCEPTANCE (d) - success evidence is subject/suite/environment keyed: flipping the
\         subject, the suite (any digest-covered field), or the environment each mints a
\         DISTINCT evidence identity; an equal triple interns to ONE id.
\   CX-* : the failure counterexample lowers to a lossless DIAG (code + expected/observed
\         survive an ENCODE/DECODE round-trip).
\
\ The test reopens package DIFFRUN (a friend) to reach the private wrappers/log helpers;
\ identity fixtures mint real ids through their owner constructors (never a raw cast) and
\ carry the diffrun-test prefix (docs/forth.md "fixtures use unique test-owned names").

require lib/test.f
require lib/string.f
require test/checker-assert.f
require maki/db/diff-runner.f
require maki/db/diff-suite.f
require maki/numpolicy.f
require maki/producer.f
require maki/config.f
require maki/target/target.f
require maki/db/obligation.f
require maki/db/budget-dim.f
require maki/db/evidence.f
require maki/db/diagnostic.f

using TFAM

package DIFFRUN

create CID-A CIDW allot           \ case-id buffer A (original case)
create CID-B CIDW allot           \ case-id buffer B (minimized case)
create CXBUF 4096 allot           \ counterexample encode/decode buffer

: MEM= ( ptr u8 ptr u8 n -- bool ) {: pa:ptr pb:ptr n:n :}
   0 begin dup n < while
      dup {: k:n :}
      pa k + c@  pb k + c@  <> if drop false exit then
      1+
   repeat drop true ;

\ ---- shared identities (REGISTER interns by content) ---------------------------------
: SUBJ-A ( -- CAD-KIND:producer-id )  s" diffrun-test/subject-habu" PRODUCER:REGISTER ;
: SUBJ-B ( -- CAD-KIND:producer-id )  s" diffrun-test/subject-habu-alt" PRODUCER:REGISTER ;
: CMP-EXACT ( -- CAD-KIND:numeric-policy-id )  NPOL-DOM:EXACT NPOL:REGISTER ;
: T1 ( -- CAD-KIND:target-id )  TARGET:SM87 ;
: C1 ( -- CAD-KIND:config-id )  s" diffrun-test/env-a" CONFIG:REGISTER ;
: C2 ( -- CAD-KIND:config-id )  s" diffrun-test/env-b" CONFIG:REGISTER ;

\ ---- deterministic scripted adapters -------------------------------------------------
\ subject(n) = n (produced), unless n = FAULT-AT (a hang/crash stand-in -> faulted).
\ reference(n) = n, unless n >= INJ-THRESH (then n+REF-OFFSET -> a mismatch under exact),
\ or REF-AVAIL is off (then skip: the off-device / no-PyTorch reference-unavailable case).
variable INJ-THRESH variable REF-OFFSET variable FAULT-AT variable REF-AVAIL

: SCRIPT-SUBJECT ( n -- run-result ) {: n:n :}
   n FAULT-AT @ = if >FAULTED exit then
   n >PRODUCED ;
: SCRIPT-REF ( n -- ref-result ) {: n:n :}
   REF-AVAIL @ 0= if >SKIP exit then
   n INJ-THRESH @ >= if n REF-OFFSET @ + >VALUE else n >VALUE then ;
: INSTALL-SCRIPT ( -- )
   [: SCRIPT-SUBJECT ;] SUBJECT!
   [: SCRIPT-REF ;] REFERENCE! ;
: SCRIPT-RESET ( -- )
   5 INJ-THRESH !  100 REF-OFFSET !  -1 FAULT-AT !  1 REF-AVAIL ! ;
: SETUP ( -- )  INSTALL-SCRIPT SCRIPT-RESET ;

\ ---- scalar checker suite fixture (exact domain, zero tolerance) ---------------------
: BUILD-SCALAR ( CAD-KIND:producer-id n -- DIFFSUITE:suite )
   {: subj:CAD-KIND:producer-id seed:n :}
   DIFFSUITE:NEW
   seed DIFFSUITE:SEED!
   subj DIFFSUITE:SUBJECT
   OBLIG-INDEPENDENCE:SELF-VERIFY DIFFSUITE:POLICY
   CMP-EXACT 0 DIFFSUITE:COMPARISON
   s" diffrun-test/norm" DIFFSUITE:NORMALIZATION
   s" diffrun-test/min" DIFFSUITE:MINIMIZER
   T1 DIFFSUITE:TARGET-NEED
   BUDGET-DIM:COMPUTE-TIME 100 DIFFSUITE:BUDGET!
   s" diffrun-test/gen" DIFFSUITE:GENERATOR+
   subj DIFFSUITE:REFERENCE+
   s" diffrun-test/prop" DIFFSUITE:PROPERTY+
   DIFFSUITE:SEAL MATCH DIFFSUITE:build-result
      ok OF ENDOF
      incomplete OF -777 throw ENDOF
      tolerance-mismatch OF -777 throw ENDOF
      reference-not-independent OF -777 throw ENDOF
   ;MATCH ;

\ ---- ACCEPTANCE (a): minimize + replay -----------------------------------------------
: A-RUN ( -- n )   SETUP  SUBJ-A 42 BUILD-SCALAR 10 RUN RUN-VERDICT>N ;   \ falsified (1)
: A-FF ( -- n )    SETUP  SUBJ-A 42 BUILD-SCALAR 10 RUN RUN-VERDICT>N drop  FIRST-FAIL-CASE ;
: A-MIN ( -- n )   SETUP  SUBJ-A 42 BUILD-SCALAR {: h:DIFFSUITE:suite :}  h 9 MINIMIZE ;
: A-MIN-SMALLER ( -- bool )
   SETUP  SUBJ-A 42 BUILD-SCALAR {: h:DIFFSUITE:suite :}  h 9 MINIMIZE  9 < ;
: A-MIN-DETERMINISTIC ( -- bool )
   SETUP  SUBJ-A 42 BUILD-SCALAR {: h:DIFFSUITE:suite :}  h 9 MINIMIZE  h 9 MINIMIZE  = ;
: A-MIN-REPLAYS ( -- bool )
   SETUP  SUBJ-A 42 BUILD-SCALAR {: h:DIFFSUITE:suite :}  h  h 9 MINIMIZE  FAILS? ;
: A-ORIGINAL-PRESERVED ( -- bool )   \ minimized case is a SEPARATE content-addressed artifact
   SETUP  SUBJ-A 42 BUILD-SCALAR {: h:DIFFSUITE:suite :}
   h 9 MINIMIZE {: pmin:n :}
   h 9    CID-A DIFFSUITE:CASE-ID
   h pmin CID-B DIFFSUITE:CASE-ID
   CID-A CID-B CIDW MEM= 0= ;

\ ---- ACCEPTANCE (b): fault taxonomy is distinct from mismatch -------------------------
: B-EXIT0 ( -- bool )    0 OUTCOME:EXITED CLASSIFY-OUTCOME ;      \ clean exit -> produced-eligible
: B-EXITN ( -- bool )    3 OUTCOME:EXITED CLASSIFY-OUTCOME ;      \ nonzero exit -> fault
: B-SIGNAL ( -- bool )   9 OUTCOME:SIGNALED CLASSIFY-OUTCOME ;    \ signal death -> fault
: B-TIMEOUT ( -- bool )  OUTCOME:TIMEOUT CLASSIFY-OUTCOME ;       \ hung, SIGKILL-reaped -> fault
: B-FAULT-VERDICT ( -- n )   \ fault at an otherwise-agreeing case -> subject-fault (2), never wrong-value
   INSTALL-SCRIPT SCRIPT-RESET  2 FAULT-AT !
   SUBJ-A 42 BUILD-SCALAR 2 CASE-VERDICT CASE-VERDICT>N ;
: B-MISMATCH-VERDICT ( -- n )   \ a genuine discrepancy is mismatch (1), distinct from fault (2)
   SETUP  SUBJ-A 42 BUILD-SCALAR 7 CASE-VERDICT CASE-VERDICT>N ;

\ ---- ACCEPTANCE (c): reference-unavailable is a recorded skip, not a mismatch ---------
: C-SKIP-VERDICT ( -- n )
   INSTALL-SCRIPT SCRIPT-RESET  0 REF-AVAIL !
   SUBJ-A 42 BUILD-SCALAR 7 CASE-VERDICT CASE-VERDICT>N ;

\ ---- ACCEPTANCE (d): evidence keyed by subject / suite / environment ------------------
: D-SAME ( -- bool )
   SUBJ-A 42 BUILD-SCALAR C1 EMIT-EVIDENCE {: e1:CAD-KIND:evidence-id :}
   SUBJ-A 42 BUILD-SCALAR C1 EMIT-EVIDENCE {: e2:CAD-KIND:evidence-id :}
   e1 e2 EVIDENCE:EQUAL? ;
: D-SUBJECT-FLIP ( -- bool )
   SUBJ-A 42 BUILD-SCALAR C1 EMIT-EVIDENCE {: e1:CAD-KIND:evidence-id :}
   SUBJ-B 42 BUILD-SCALAR C1 EMIT-EVIDENCE {: e2:CAD-KIND:evidence-id :}
   e1 e2 EVIDENCE:EQUAL? 0= ;
: D-SUITE-FLIP ( -- bool )
   SUBJ-A 42 BUILD-SCALAR C1 EMIT-EVIDENCE {: e1:CAD-KIND:evidence-id :}
   SUBJ-A 99 BUILD-SCALAR C1 EMIT-EVIDENCE {: e2:CAD-KIND:evidence-id :}
   e1 e2 EVIDENCE:EQUAL? 0= ;
: D-ENV-FLIP ( -- bool )
   SUBJ-A 42 BUILD-SCALAR C1 EMIT-EVIDENCE {: e1:CAD-KIND:evidence-id :}
   SUBJ-A 42 BUILD-SCALAR C2 EMIT-EVIDENCE {: e2:CAD-KIND:evidence-id :}
   e1 e2 EVIDENCE:EQUAL? 0= ;

\ ---- counterexample: lossless DIAG lowering ------------------------------------------
: CX-BUILD ( -- DIAG:build-result )   \ discrepancy at case 5: subject 5, reference 105
   SUBJ-A 42 BUILD-SCALAR 5 5 105 EMIT-COUNTEREXAMPLE ;
: CX-CODE ( -- n )
   CX-BUILD MATCH DIAG:build-result
      ok OF DIAG:CODE@ ENDOF
      missing-owner OF -778 throw ENDOF
      missing-reproduction OF -778 throw ENDOF
   ;MATCH ;
: CX-ENCODE ( DIAG:diagnostic -- ptr u8 n ) {: d:DIAG:diagnostic :}
   d CXBUF 4096 DIAG:ENCODE {: n:n :}  CXBUF n ;
: DECODED-OK? ( DIAG:diagnostic -- bool ) {: d2:DIAG:diagnostic :}
   d2 DIAG:CODE@ E-DIFFRUN-MISMATCH =
   d2 DIAG:EXPECTED-COUNT 1 = and
   d2 0 DIAG:EXPECTED@ s" ref=105" STR= and ;
: CX-DECODE-OK? ( ptr u8 n -- bool )
   DIAG:DECODE MATCH DIAG:decode-result
      ok OF DECODED-OK? ENDOF
      malformed OF false ENDOF
      noncanonical OF false ENDOF
      bounds OF false ENDOF
      duplicate OF false ENDOF
      unknown-required OF false ENDOF
   ;MATCH ;
: CX-ROUNDTRIP ( -- bool )
   CX-BUILD MATCH DIAG:build-result
      ok OF CX-ENCODE CX-DECODE-OK? ENDOF
      missing-owner OF false ENDOF
      missing-reproduction OF false ENDOF
   ;MATCH ;

\ ---- every variant constructs and dispatches through MATCH ---------------------------
\ The acceptance legs above reach the two payload families only through the scripted
\ adapters. These construct each variant DIRECTLY through the private production wrappers
\ and read it straight back, which is what proves the named payload FIELDs (`subj` on
\ run-result's produced, `ref` on ref-result's value) bind in declaration order. Each
\ payload arm binds a TYPED local and returns the recovered scalar, and each family is
\ read back at TWO distinct non-zero values, so a payload the constructor dropped, zeroed
\ or replaced with a constant fails instead of passing.
\
\ Construction is factored into one typed word per variant because the checker requires
\ MATCH's scrutinee to be a concretely instantiated family value: a single word that both
\ constructs and matches is refused, and the diagnostic names the family token as an
\ undefined word. That refusal predates this migration (it reproduces identically on the
\ legacy declaration) and is reported separately.
: TT-MK-PROD ( n -- run-result )   >PRODUCED ;
: TT-MK-FLT ( -- run-result )      >FAULTED ;
: TT-MK-VAL ( n -- ref-result )    >VALUE ;
: TT-MK-SKIP ( -- ref-result )     >SKIP ;

: TT-RR-ARM ( run-result -- n )                  \ 1 produced, 2 faulted
   MATCH run-result
      produced OF drop 1 ENDOF
      faulted  OF 2 ENDOF
   ;MATCH ;
: TT-RR-SUBJ ( run-result -- n )                 \ the produced scalar, else -1
   MATCH run-result
      produced OF {: subj:n :} subj ENDOF
      faulted  OF -1 ENDOF
   ;MATCH ;
: TT-FR-ARM ( ref-result -- n )                  \ 1 value, 2 skip
   MATCH ref-result
      value OF drop 1 ENDOF
      skip  OF 2 ENDOF
   ;MATCH ;
: TT-FR-REF ( ref-result -- n )                  \ the reference scalar, else -1
   MATCH ref-result
      value OF {: ref:n :} ref ENDOF
      skip  OF -1 ENDOF
   ;MATCH ;

: TT-RR-RT-ARM ( -- n )    7 TT-MK-PROD TT-RR-ARM ;
: TT-RR-RT-7 ( -- n )      7 TT-MK-PROD TT-RR-SUBJ ;      \ two distinct values: not a constant
: TT-RR-RT-9 ( -- n )      9 TT-MK-PROD TT-RR-SUBJ ;
: TT-RR-RT-FLT ( -- n )    TT-MK-FLT TT-RR-ARM ;
: TT-RR-FLT-SUBJ ( -- n )  TT-MK-FLT TT-RR-SUBJ ;         \ a payloadless arm carries no scalar
: TT-FR-RT-ARM ( -- n )    11 TT-MK-VAL TT-FR-ARM ;
: TT-FR-RT-11 ( -- n )     11 TT-MK-VAL TT-FR-REF ;
: TT-FR-RT-13 ( -- n )     13 TT-MK-VAL TT-FR-REF ;
: TT-FR-RT-SKIP ( -- n )   TT-MK-SKIP TT-FR-ARM ;
: TT-FR-SKIP-REF ( -- n )  TT-MK-SKIP TT-FR-REF ;

\ The two payloadless families are read back through their public ordinal projections,
\ which are MATCH-based, so these eight values pin the declared CASE ORDER: exchanging any
\ two cases in either declaration renumbers the tags and turns these red.
: TT-CV-AGREE ( -- n )    >AGREE CASE-VERDICT>N ;
: TT-CV-MIS ( -- n )      >MISMATCH CASE-VERDICT>N ;
: TT-CV-SF ( -- n )       >SUBJECT-FAULT CASE-VERDICT>N ;
: TT-CV-RS ( -- n )       >REFERENCE-SKIP CASE-VERDICT>N ;
: TT-RV-VER ( -- n )      >VERIFIED RUN-VERDICT>N ;
: TT-RV-FAL ( -- n )      >FALSIFIED RUN-VERDICT>N ;
: TT-RV-SFD ( -- n )      >SUBJECT-FAULTED RUN-VERDICT>N ;
: TT-RV-SKP ( -- n )      >SKIPPED RUN-VERDICT>N ;

T-RESET

\ ---- ACCEPTANCE (a) ------------------------------------------------------------------
A-RUN 1 T=                 \ a discrepancy falsifies the run
A-FF 5 T=                  \ first failing case is the earliest mismatch
A-MIN 5 T=                 \ minimize a non-minimal failing case (9) to its minimum (5)
A-MIN-SMALLER TTRUE        \ minimized parameter is strictly smaller than the original
A-MIN-DETERMINISTIC TTRUE  \ the minimizer is a pure function (same minimized case both runs)
A-MIN-REPLAYS TTRUE        \ the minimized case still mismatches on replay
A-ORIGINAL-PRESERVED TTRUE \ minimized case is a distinct content-addressed artifact

\ ---- ACCEPTANCE (b) ------------------------------------------------------------------
B-EXIT0 TTRUE
B-EXITN TFALSE
B-SIGNAL TFALSE
B-TIMEOUT TFALSE
B-FAULT-VERDICT 2 T=       \ fault at an agreeing case grades subject-fault, not agree/mismatch
B-MISMATCH-VERDICT 1 T=    \ a genuine discrepancy grades mismatch, distinct from fault

\ ---- ACCEPTANCE (c) ------------------------------------------------------------------
C-SKIP-VERDICT 3 T=        \ reference unavailable -> reference-skip recorded, not a mismatch

\ ---- ACCEPTANCE (d) ------------------------------------------------------------------
D-SAME TTRUE
D-SUBJECT-FLIP TTRUE
D-SUITE-FLIP TTRUE
D-ENV-FLIP TTRUE

\ ---- counterexample lossless ---------------------------------------------------------
CX-CODE -5398 T=
CX-ROUNDTRIP TTRUE

\ ---- the four families construct and dispatch through MATCH --------------------------
TT-RR-RT-ARM 1 T=          \ produced dispatches to its own arm
TT-RR-RT-7 7 T=            \ and carries its `subj` payload through unchanged ...
TT-RR-RT-9 9 T=            \ ... at a second distinct value, so it is not a constant
TT-RR-RT-FLT 2 T=          \ faulted dispatches to its own arm
TT-RR-FLT-SUBJ -1 T=       \ the no-payload arm of TT-RR-SUBJ is live
TT-FR-RT-ARM 1 T=          \ value dispatches to its own arm
TT-FR-RT-11 11 T=          \ and carries its `ref` payload through unchanged ...
TT-FR-RT-13 13 T=          \ ... at a second distinct value
TT-FR-RT-SKIP 2 T=         \ skip dispatches to its own arm
TT-FR-SKIP-REF -1 T=       \ the no-payload arm of TT-FR-REF is live
TT-CV-AGREE 0 T=           \ compact case order: agree=0 ...
TT-CV-MIS 1 T=
TT-CV-SF 2 T=
TT-CV-RS 3 T=              \ ... reference-skip=3
TT-RV-VER 0 T=             \ compact case order: verified=0 ...
TT-RV-FAL 1 T=
TT-RV-SFD 2 T=
TT-RV-SKP 3 T=             \ ... skipped=3

;package

\ ---- how the four families are DECLARED (dot habu-migrate-diff-runner-e4257b87) ------
\ run-result and ref-result carry a payload, so they use the full ENUM form and the type
\ registry records them as general sums (kind 2). case-verdict and run-verdict are entirely
\ payloadless, so they use the compact form and are recorded as enum families (kind 3) -
\ the deliberate kind change ruling R1 asks for, and the reason the census baseline gains a
\ row for each of them. Compact and sum spellings are both one cell wide here and give the
\ same MATCH surface, so no consumer can tell them apart by behaviour; that is precisely
\ why the recorded kind, arity, width, visibility, case order and generated constructor
\ package are pinned below, read LIVE out of the family registry through the read-only
\ accessors the checker publishes for public-signature tooling (src/core/checker.f). Writing
\ any of these declarations back to SUMTYPE, or moving a payloadless one to the full form,
\ changes the recorded kind and turns this suite red; exchanging two cases changes the
\ pinned order.
\
\ These pins and the shape decoys own their own test package: nothing here needs the
\ production package's private wrappers, so no test-only word is added to DIFFRUN.
package DIFFRUN-TEST

: YES ( ptr u8 n -- )   CHECK-QUIET-CANDIDATE! -1 T= ;
: NO  ( ptr u8 n -- )   CHECK-QUIET-CANDIDATE!  0 T= ;

\ the four (tail, constructor package) identities this file pins. REFLECT
\ (test/checker-assert.f) does the reading; a family is named by its tail plus the
\ constructor package its variants carry, so REFLECT:FAMS = 1 below is also the proof
\ that each identity resolves exactly one registered family.
: RUNR$ ( -- ptr u8 n ptr u8 n )   s" run-result" s" DIFFRUN-RUN--RESULT" ;
: REFR$ ( -- ptr u8 n ptr u8 n )   s" ref-result" s" DIFFRUN-REF--RESULT" ;
: CV$ ( -- ptr u8 n ptr u8 n )     s" case-verdict" s" DIFFRUN-CASE--VERDICT" ;
: RV$ ( -- ptr u8 n ptr u8 n )     s" run-verdict" s" DIFFRUN-RUN--VERDICT" ;

public

\ twin is DIFFRUN:run-result's SHAPE under a different name: same arity, same two cases in
\ the same order, same named payload field. It proves runner-result identity is NOMINAL -
\ two identically shaped families never unify, in either direction. Public, so it publishes
\ constructors for the positive control; the generated package is DIFFRUN--TEST-TWIN, well
\ inside the 32-byte readable-spelling limit (TF-CTOR-NAME-LIMIT, src/core/type-family.f).
ENUM twin 0
   VARIANT produced FIELD subj n ;VARIANT
   VARIANT faulted ;VARIANT
;ENUM

private

\ The two decoys are the payloadless counterpart: same compact form, same four case names
\ in the same order, same one-cell width as the family each shadows. Private, so they
\ publish no constructors and the negatives below are MATCH-based in both directions.
ENUM cv-decoy agree mismatch subject-fault reference-skip ;ENUM
ENUM rv-decoy verified falsified subject-faulted skipped ;ENUM

\ ---- live registry: run-result stays a full-form sum with two cases -------------------
RUNR$ REFLECT:FAMS 1 T=
RUNR$ REFLECT:KIND TK-SUM T=       \ a payload family stays a general sum ...
RUNR$ REFLECT:KIND TK-ENUM = 0 T=  \ ... and is NOT recorded as an enum
RUNR$ REFLECT:ARITY 0 T=
RUNR$ REFLECT:WIDTH 2 T=           \ tag + one payload cell
RUNR$ REFLECT:VIS 1 T=
RUNR$ REFLECT:VARS 2 T=
RUNR$ 0 REFLECT:ARM$ s" produced" T$=           \ case order fixes the tags
RUNR$ 1 REFLECT:ARM$ s" faulted" T$=
RUNR$ 0 REFLECT:ARM-CTOR$ s" DIFFRUN-RUN--RESULT" T$=
RUNR$ 1 REFLECT:ARM-CTOR$ s" DIFFRUN-RUN--RESULT" T$=

\ ---- live registry: ref-result, the same shape under its own name ---------------------
REFR$ REFLECT:FAMS 1 T=
REFR$ REFLECT:KIND TK-SUM T=
REFR$ REFLECT:ARITY 0 T=
REFR$ REFLECT:WIDTH 2 T=
REFR$ REFLECT:VIS 1 T=
REFR$ REFLECT:VARS 2 T=
REFR$ 0 REFLECT:ARM$ s" value" T$=
REFR$ 1 REFLECT:ARM$ s" skip" T$=
REFR$ 0 REFLECT:ARM-CTOR$ s" DIFFRUN-REF--RESULT" T$=

\ ---- live registry: case-verdict is now a COMPACT enum family ------------------------
CV$ REFLECT:FAMS 1 T=
CV$ REFLECT:KIND TK-ENUM T=        \ the pinned ruling R1 kind ...
CV$ REFLECT:KIND TK-SUM = 0 T=     \ ... and no longer a general sum
CV$ REFLECT:ARITY 0 T=
CV$ REFLECT:WIDTH 1 T=             \ one cell, the same width the sum form had
CV$ REFLECT:VIS 1 T=
CV$ REFLECT:VARS 4 T=
CV$ 0 REFLECT:ARM$ s" agree" T$=
CV$ 1 REFLECT:ARM$ s" mismatch" T$=
CV$ 2 REFLECT:ARM$ s" subject-fault" T$=
CV$ 3 REFLECT:ARM$ s" reference-skip" T$=
CV$ 0 REFLECT:ARM-CTOR$ s" DIFFRUN-CASE--VERDICT" T$=
CV$ 3 REFLECT:ARM-CTOR$ s" DIFFRUN-CASE--VERDICT" T$=

\ ---- live registry: run-verdict, the second compact family ---------------------------
RV$ REFLECT:FAMS 1 T=
RV$ REFLECT:KIND TK-ENUM T=
RV$ REFLECT:KIND TK-SUM = 0 T=
RV$ REFLECT:ARITY 0 T=
RV$ REFLECT:WIDTH 1 T=
RV$ REFLECT:VIS 1 T=
RV$ REFLECT:VARS 4 T=
RV$ 0 REFLECT:ARM$ s" verified" T$=
RV$ 1 REFLECT:ARM$ s" falsified" T$=
RV$ 2 REFLECT:ARM$ s" subject-faulted" T$=
RV$ 3 REFLECT:ARM$ s" skipped" T$=
RV$ 0 REFLECT:ARM-CTOR$ s" DIFFRUN-RUN--VERDICT" T$=
RV$ 3 REFLECT:ARM-CTOR$ s" DIFFRUN-RUN--VERDICT" T$=

\ ---- generated constructors: exact spelling + exact effect ---------------------------
\ The SPELLING is load-bearing: the checker answers 1 (uncheckable) for a name it cannot
\ resolve, and YES demands -1, so a -1 means the checker resolved EXACTLY this constructor
\ name; NO demands 0, which it can only reach after resolving the name and refusing the
\ types. maki/db/diff-case-store.f and maki/db/diff-case-store-xproc-child.f construct and
\ MATCH these families across a package boundary, so drift here would break consumers this
\ suite never loads.
s" RR-C-PROD ( n -- DIFFRUN:run-result ) DIFFRUN-RUN--RESULT:PRODUCED" YES
s" RR-C-FLT ( -- DIFFRUN:run-result ) DIFFRUN-RUN--RESULT:FAULTED" YES
s" FR-C-VAL ( n -- DIFFRUN:ref-result ) DIFFRUN-REF--RESULT:VALUE" YES
s" FR-C-SKIP ( -- DIFFRUN:ref-result ) DIFFRUN-REF--RESULT:SKIP" YES
s" CV-C-AGR ( -- DIFFRUN:case-verdict ) DIFFRUN-CASE--VERDICT:AGREE" YES
s" CV-C-MIS ( -- DIFFRUN:case-verdict ) DIFFRUN-CASE--VERDICT:MISMATCH" YES
s" CV-C-SF ( -- DIFFRUN:case-verdict ) DIFFRUN-CASE--VERDICT:SUBJECT-FAULT" YES
s" CV-C-RS ( -- DIFFRUN:case-verdict ) DIFFRUN-CASE--VERDICT:REFERENCE-SKIP" YES
s" RV-C-VER ( -- DIFFRUN:run-verdict ) DIFFRUN-RUN--VERDICT:VERIFIED" YES
s" RV-C-FAL ( -- DIFFRUN:run-verdict ) DIFFRUN-RUN--VERDICT:FALSIFIED" YES
s" RV-C-SFD ( -- DIFFRUN:run-verdict ) DIFFRUN-RUN--VERDICT:SUBJECT-FAULTED" YES
s" RV-C-SKP ( -- DIFFRUN:run-verdict ) DIFFRUN-RUN--VERDICT:SKIPPED" YES
\ Forge negatives: the produced/value payload is mandatory and is not a bare scalar, a
\ payloadless case takes no payload, and a payloadless family's constructor is not an n.
s" RR-F-NONE ( -- DIFFRUN:run-result ) DIFFRUN-RUN--RESULT:PRODUCED" NO
s" RR-F-BARE ( n -- n ) DIFFRUN-RUN--RESULT:PRODUCED" NO
s" RR-F-PAY ( n -- DIFFRUN:run-result ) DIFFRUN-RUN--RESULT:FAULTED" NO
s" FR-F-NONE ( -- DIFFRUN:ref-result ) DIFFRUN-REF--RESULT:VALUE" NO
s" CV-F-N ( -- n ) DIFFRUN-CASE--VERDICT:AGREE" NO
s" CV-F-PAY ( n -- DIFFRUN:case-verdict ) DIFFRUN-CASE--VERDICT:AGREE" NO
\ Cross-family negatives between the PRODUCTION families. run-result and ref-result are
\ structurally identical (arity 0, one n-payload case plus one payloadless case) and the two
\ verdict families are both four payloadless cases, so these four are the real
\ same-shape-different-name test, not a synthetic one.
s" RR-F-XFAM ( n -- DIFFRUN:ref-result ) DIFFRUN-RUN--RESULT:PRODUCED" NO
s" FR-F-XFAM ( n -- DIFFRUN:run-result ) DIFFRUN-REF--RESULT:VALUE" NO
s" RV-F-XFAM ( -- DIFFRUN:case-verdict ) DIFFRUN-RUN--VERDICT:VERIFIED" NO
s" CV-F-XFAM ( -- DIFFRUN:run-verdict ) DIFFRUN-CASE--VERDICT:AGREE" NO
\ The public twin: same shape, different name, does not unify in either direction.
s" TW-C ( n -- twin ) DIFFRUN--TEST-TWIN:PRODUCED" YES
s" TW-X1 ( n -- twin ) DIFFRUN-RUN--RESULT:PRODUCED" NO
s" TW-X2 ( n -- DIFFRUN:run-result ) DIFFRUN--TEST-TWIN:PRODUCED" NO
\ The compact decoys: MATCH accepts each family only for its own scrutinee. The first
\ candidate of each pair is the positive control, so a refusal below cannot be a refusal of
\ the shape. A raw cell is not a verdict at the MATCH entry either.
s" CV-M-OK ( DIFFRUN:case-verdict -- ) MATCH DIFFRUN:case-verdict agree OF ENDOF mismatch OF ENDOF subject-fault OF ENDOF reference-skip OF ENDOF ;MATCH" YES
s" CV-M-N ( n -- ) MATCH DIFFRUN:case-verdict agree OF ENDOF mismatch OF ENDOF subject-fault OF ENDOF reference-skip OF ENDOF ;MATCH" NO
s" CV-D-OK ( cv-decoy -- ) MATCH cv-decoy agree OF ENDOF mismatch OF ENDOF subject-fault OF ENDOF reference-skip OF ENDOF ;MATCH" YES
s" CV-D-X1 ( cv-decoy -- ) MATCH DIFFRUN:case-verdict agree OF ENDOF mismatch OF ENDOF subject-fault OF ENDOF reference-skip OF ENDOF ;MATCH" NO
s" CV-D-X2 ( DIFFRUN:case-verdict -- ) MATCH cv-decoy agree OF ENDOF mismatch OF ENDOF subject-fault OF ENDOF reference-skip OF ENDOF ;MATCH" NO
s" RV-M-OK ( DIFFRUN:run-verdict -- ) MATCH DIFFRUN:run-verdict verified OF ENDOF falsified OF ENDOF subject-faulted OF ENDOF skipped OF ENDOF ;MATCH" YES
s" RV-D-OK ( rv-decoy -- ) MATCH rv-decoy verified OF ENDOF falsified OF ENDOF subject-faulted OF ENDOF skipped OF ENDOF ;MATCH" YES
s" RV-D-X1 ( rv-decoy -- ) MATCH DIFFRUN:run-verdict verified OF ENDOF falsified OF ENDOF subject-faulted OF ENDOF skipped OF ENDOF ;MATCH" NO
s" RV-D-X2 ( DIFFRUN:run-verdict -- ) MATCH rv-decoy verified OF ENDOF falsified OF ENDOF subject-faulted OF ENDOF skipped OF ENDOF ;MATCH" NO

;package

T-REPORT

;using
