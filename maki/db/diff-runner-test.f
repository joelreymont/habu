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

T-REPORT

;package
