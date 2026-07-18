\ maki/db/diff-runner-tensor-test.f - acceptance for the TENSOR forward differential
\ suite (maki/db/diff-runner-tensor.f; dot habu-v2-differential-runner-13359019).
\
\ Drives the tensor runner over a float forward suite (relative domain, 1e-5 elementwise
\ bound) with deterministic IN-PROCESS scripted tensor adapters, proving the dot's
\ acceptance for the tensor leg (all sum/product values produced+consumed INSIDE colon
\ words, never on the interpret-mode stack):
\   TA-* : (a) an injected elementwise mismatch MINIMIZES to its minimal counterexample and
\          REPLAYS deterministically; the minimized case is a SEPARATE content-addressed
\          artifact (CASE-ID(p') != CASE-ID(p)); the original failing case is untouched.
\   TB-* : (b) a scripted fault at an otherwise-agreeing case grades subject-fault (2),
\          DISTINCT from a genuine elementwise mismatch (1); an in-tolerance tensor agrees.
\   TC-* : (c) a reference-legs SKIP is recorded (reference-skip), never a mismatch.
\   TD-* : (d) success evidence is subject/suite/environment keyed (the reused suite-level
\          EMIT-EVIDENCE): flipping subject / suite / environment mints a DISTINCT id.
\   TCMP-* : the elementwise comparator - within tolerance agrees, beyond mismatches, and
\          an exact domain requires bit-exact elementwise equality.
\   TCX-*  : the failure counterexample lowers to a lossless DIAG (code + first-diff element
\          expected/observed survive an ENCODE/DECODE round-trip).
\
\ Reopens package DIFFRUN (a friend) for the tensor surface + the private run-result
\ wrappers; the REAL spawned-child + subject-source-injection proof is
\ maki/db/diff-runner-inject-test.f. Identity fixtures mint real ids through their owner
\ constructors; names carry the difftensor-test prefix.

require lib/test.f
require lib/string.f
require lib/float.f
require maki/db/diff-runner-tensor.f
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

4 constant TLEN                   \ tensor length under test
10000 constant TTOL               \ suite u64 tolerance = 1e-5 elementwise bound (nano-units)
create TCID-A CIDW allot          \ case-id buffer A (original case)
create TCID-B CIDW allot          \ case-id buffer B (minimized case)
create TCXBUF 2048 allot          \ counterexample encode/decode buffer

: TMEM= ( ptr u8 ptr u8 n -- bool ) {: pa:ptr pb:ptr n:n :}
   0 begin dup n < while
      dup {: k:n :}
      pa k + c@  pb k + c@  <> if drop false exit then
      1+
   repeat drop true ;

\ ---- shared identities ---------------------------------------------------------------
: T-SUB-A ( -- CAD-KIND:producer-id )  s" difftensor-test/subject-habu" PRODUCER:REGISTER ;
: T-SUB-B ( -- CAD-KIND:producer-id )  s" difftensor-test/subject-habu-alt" PRODUCER:REGISTER ;
: CMP-REL ( -- CAD-KIND:numeric-policy-id )  NPOL-DOM:RELATIVE NPOL:REGISTER ;
: T-TGT ( -- CAD-KIND:target-id )  TARGET:SM87 ;
: T-ENV-A ( -- CAD-KIND:config-id )  s" difftensor-test/env-a" CONFIG:REGISTER ;
: T-ENV-B ( -- CAD-KIND:config-id )  s" difftensor-test/env-b" CONFIG:REGISTER ;

\ ---- deterministic scripted tensor adapters ------------------------------------------
\ subject(n) fills SUBJ-T[i] = i (float), unless n = FAULT-AT (a hang/crash stand-in).
\ reference(n) fills REF-T[i] = i (agreeing), unless n >= INJ-THRESH (then element 0 is
\ perturbed by REF-DELTA >> tolerance -> mismatch), or REF-AVAIL is off (then skip).
variable T-INJ-THRESH variable T-FAULT-AT variable T-REF-AVAIL
: T-REF-DELTA ( -- r )   0.001 ;      \ well beyond the 1e-5 bound

: FILL-SUBJ ( -- )   TLEN 0 ?do  i s>f  i SUBJ-T!  loop ;
: FILL-REF ( n -- ) {: n:n :}
   TLEN 0 ?do  i s>f  i REF-T!  loop
   n T-INJ-THRESH @ >= if  0 REF-T@ T-REF-DELTA f+  0 REF-T!  then ;

: T-SCRIPT-SUBJECT ( n -- run-result ) {: n:n :}
   n T-FAULT-AT @ = if >FAULTED exit then
   FILL-SUBJ  TLEN >PRODUCED ;
: T-SCRIPT-REF ( n -- ref-result ) {: n:n :}
   T-REF-AVAIL @ 0= if >SKIP exit then
   n FILL-REF  TLEN >VALUE ;
: T-INSTALL ( -- )
   [: T-SCRIPT-SUBJECT ;] T-SUBJECT!
   [: T-SCRIPT-REF ;] T-REFERENCE! ;
: T-SCRIPT-RESET ( -- )   5 T-INJ-THRESH !  -1 T-FAULT-AT !  1 T-REF-AVAIL ! ;
: T-SETUP ( -- )   T-INSTALL T-SCRIPT-RESET ;

\ ---- tensor suite fixture (relative domain, positive tolerance) ----------------------
: T-SEAL-OK ( -- DIFFSUITE:suite )
   DIFFSUITE:SEAL MATCH DIFFSUITE:build-result
      ok OF ENDOF
      incomplete OF -777 throw ENDOF
      tolerance-mismatch OF -777 throw ENDOF
      reference-not-independent OF -777 throw ENDOF
   ;MATCH ;
: BUILD-TSUITE ( CAD-KIND:producer-id n -- DIFFSUITE:suite )
   {: subj:CAD-KIND:producer-id seed:n :}
   DIFFSUITE:NEW
   seed DIFFSUITE:SEED!
   subj DIFFSUITE:SUBJECT
   OBLIG-INDEPENDENCE:SELF-VERIFY DIFFSUITE:POLICY
   CMP-REL TTOL DIFFSUITE:COMPARISON
   s" difftensor-test/norm" DIFFSUITE:NORMALIZATION
   s" difftensor-test/min" DIFFSUITE:MINIMIZER
   T-TGT DIFFSUITE:TARGET-NEED
   BUDGET-DIM:COMPUTE-TIME 100 DIFFSUITE:BUDGET!
   s" difftensor-test/gen" DIFFSUITE:GENERATOR+
   subj DIFFSUITE:REFERENCE+
   s" difftensor-test/prop" DIFFSUITE:PROPERTY+
   T-SEAL-OK ;

\ ---- ACCEPTANCE (a): minimize + replay -----------------------------------------------
: TA-RUN ( -- n )   T-SETUP  T-SUB-A 42 BUILD-TSUITE 10 T-RUN RUN-VERDICT>N ;
: TA-FF ( -- n )    T-SETUP  T-SUB-A 42 BUILD-TSUITE 10 T-RUN RUN-VERDICT>N drop  FIRST-FAIL-CASE ;
: TA-MIN ( -- n )   T-SETUP  T-SUB-A 42 BUILD-TSUITE {: h:DIFFSUITE:suite :}  h 9 T-MINIMIZE ;
: TA-MIN-SMALLER ( -- bool )
   T-SETUP  T-SUB-A 42 BUILD-TSUITE {: h:DIFFSUITE:suite :}  h 9 T-MINIMIZE  9 < ;
: TA-MIN-DETERMINISTIC ( -- bool )
   T-SETUP  T-SUB-A 42 BUILD-TSUITE {: h:DIFFSUITE:suite :}  h 9 T-MINIMIZE  h 9 T-MINIMIZE  = ;
: TA-MIN-REPLAYS ( -- bool )
   T-SETUP  T-SUB-A 42 BUILD-TSUITE {: h:DIFFSUITE:suite :}  h  h 9 T-MINIMIZE  T-FAILS? ;
: TA-ORIGINAL-PRESERVED ( -- bool )
   T-SETUP  T-SUB-A 42 BUILD-TSUITE {: h:DIFFSUITE:suite :}
   h 9 T-MINIMIZE {: pmin:n :}
   h 9    TCID-A DIFFSUITE:CASE-ID
   h pmin TCID-B DIFFSUITE:CASE-ID
   TCID-A TCID-B CIDW TMEM= 0= ;

\ ---- ACCEPTANCE (b): fault taxonomy distinct from mismatch ----------------------------
: TB-FAULT-VERDICT ( -- n )
   T-INSTALL T-SCRIPT-RESET  2 T-FAULT-AT !
   T-SUB-A 42 BUILD-TSUITE 2 T-CASE-VERDICT CASE-VERDICT>N ;
: TB-MISMATCH-VERDICT ( -- n )
   T-SETUP  T-SUB-A 42 BUILD-TSUITE 7 T-CASE-VERDICT CASE-VERDICT>N ;
: TB-AGREE-VERDICT ( -- n )     \ an in-tolerance case (n < threshold) agrees
   T-SETUP  T-SUB-A 42 BUILD-TSUITE 3 T-CASE-VERDICT CASE-VERDICT>N ;

\ ---- ACCEPTANCE (c): reference-unavailable is a recorded skip -------------------------
: TC-SKIP-VERDICT ( -- n )
   T-INSTALL T-SCRIPT-RESET  0 T-REF-AVAIL !
   T-SUB-A 42 BUILD-TSUITE 7 T-CASE-VERDICT CASE-VERDICT>N ;

\ ---- ACCEPTANCE (d): evidence keyed by subject / suite / environment ------------------
: TD-SAME ( -- bool )
   T-SUB-A 42 BUILD-TSUITE T-ENV-A EMIT-EVIDENCE {: e1:CAD-KIND:evidence-id :}
   T-SUB-A 42 BUILD-TSUITE T-ENV-A EMIT-EVIDENCE {: e2:CAD-KIND:evidence-id :}
   e1 e2 EVIDENCE:EQUAL? ;
: TD-SUBJECT-FLIP ( -- bool )
   T-SUB-A 42 BUILD-TSUITE T-ENV-A EMIT-EVIDENCE {: e1:CAD-KIND:evidence-id :}
   T-SUB-B 42 BUILD-TSUITE T-ENV-A EMIT-EVIDENCE {: e2:CAD-KIND:evidence-id :}
   e1 e2 EVIDENCE:EQUAL? 0= ;
: TD-SUITE-FLIP ( -- bool )
   T-SUB-A 42 BUILD-TSUITE T-ENV-A EMIT-EVIDENCE {: e1:CAD-KIND:evidence-id :}
   T-SUB-A 99 BUILD-TSUITE T-ENV-A EMIT-EVIDENCE {: e2:CAD-KIND:evidence-id :}
   e1 e2 EVIDENCE:EQUAL? 0= ;
: TD-ENV-FLIP ( -- bool )
   T-SUB-A 42 BUILD-TSUITE T-ENV-A EMIT-EVIDENCE {: e1:CAD-KIND:evidence-id :}
   T-SUB-A 42 BUILD-TSUITE T-ENV-B EMIT-EVIDENCE {: e2:CAD-KIND:evidence-id :}
   e1 e2 EVIDENCE:EQUAL? 0= ;

\ ---- elementwise comparator legs -----------------------------------------------------
: FILL-EQUAL ( -- )   TLEN 0 ?do  i s>f i SUBJ-T!  i s>f i REF-T!  loop ;
: TCMP-WITHIN ( -- bool )   \ identical tensors agree under an approximate bound
   FILL-EQUAL  TLEN NPOL-DOM:RELATIVE TTOL T-CLOSE? ;
: TCMP-BEYOND ( -- bool )   \ one element beyond the bound -> not close
   FILL-EQUAL  0 REF-T@ 0.001 f+ 0 REF-T!  TLEN NPOL-DOM:RELATIVE TTOL T-CLOSE? ;
: TCMP-EXACT-EQ ( -- bool ) \ exact domain: bit-equal tensors are close
   FILL-EQUAL  TLEN NPOL-DOM:EXACT 0 T-CLOSE? ;
: TCMP-EXACT-NEQ ( -- bool ) \ exact domain: any nonzero delta is not close
   FILL-EQUAL  0 REF-T@ 0.000001 f+ 0 REF-T!  TLEN NPOL-DOM:EXACT 0 T-CLOSE? ;

\ ---- counterexample: lossless DIAG lowering ------------------------------------------
: TCX-BUILD ( -- DIAG:build-result )
   T-SETUP  T-SUB-A 42 BUILD-TSUITE {: h:DIFFSUITE:suite :}
   h 7 T-CASE-VERDICT drop                    \ fill SUBJ-T / REF-T with case 7's mismatch
   h 7 TLEN T-EMIT-COUNTEREXAMPLE ;
: TCX-CODE ( -- n )
   TCX-BUILD MATCH DIAG:build-result
      ok OF DIAG:CODE@ ENDOF
      missing-owner OF -778 throw ENDOF
      missing-reproduction OF -778 throw ENDOF
   ;MATCH ;
: TCX-ENCODE ( DIAG:diagnostic -- ptr u8 n ) {: d:DIAG:diagnostic :}
   d TCXBUF 4096 DIAG:ENCODE {: n:n :}  TCXBUF n ;
: TCX-DECODED-OK? ( DIAG:diagnostic -- bool ) {: d2:DIAG:diagnostic :}
   d2 DIAG:CODE@ E-DIFFRUN-MISMATCH =
   d2 DIAG:EXPECTED-COUNT 1 = and
   d2 0 DIAG:EXPECTED@ s" ref=0.001000" STR= and ;
: TCX-DECODE-OK? ( ptr u8 n -- bool )
   DIAG:DECODE MATCH DIAG:decode-result
      ok OF TCX-DECODED-OK? ENDOF
      malformed OF false ENDOF
      noncanonical OF false ENDOF
      bounds OF false ENDOF
      duplicate OF false ENDOF
      unknown-required OF false ENDOF
   ;MATCH ;
: TCX-ROUNDTRIP ( -- bool )
   TCX-BUILD MATCH DIAG:build-result
      ok OF TCX-ENCODE TCX-DECODE-OK? ENDOF
      missing-owner OF false ENDOF
      missing-reproduction OF false ENDOF
   ;MATCH ;

T-RESET

\ ---- ACCEPTANCE (a) ------------------------------------------------------------------
TA-RUN 1 T=                 \ an elementwise discrepancy falsifies the run
TA-FF 5 T=                  \ first failing case is the earliest mismatch
TA-MIN 5 T=                 \ minimize a non-minimal failing case (9) to its minimum (5)
TA-MIN-SMALLER TTRUE
TA-MIN-DETERMINISTIC TTRUE
TA-MIN-REPLAYS TTRUE
TA-ORIGINAL-PRESERVED TTRUE

\ ---- ACCEPTANCE (b) ------------------------------------------------------------------
TB-FAULT-VERDICT 2 T=       \ fault at an agreeing case grades subject-fault, not mismatch
TB-MISMATCH-VERDICT 1 T=    \ an elementwise discrepancy grades mismatch, distinct from fault
TB-AGREE-VERDICT 0 T=       \ an in-tolerance tensor agrees

\ ---- ACCEPTANCE (c) ------------------------------------------------------------------
TC-SKIP-VERDICT 3 T=

\ ---- ACCEPTANCE (d) ------------------------------------------------------------------
TD-SAME TTRUE
TD-SUBJECT-FLIP TTRUE
TD-SUITE-FLIP TTRUE
TD-ENV-FLIP TTRUE

\ ---- comparator ----------------------------------------------------------------------
TCMP-WITHIN TTRUE
TCMP-BEYOND TFALSE
TCMP-EXACT-EQ TTRUE
TCMP-EXACT-NEQ TFALSE

\ ---- counterexample lossless ---------------------------------------------------------
TCX-CODE -5398 T=
TCX-ROUNDTRIP TTRUE

T-REPORT

;package
