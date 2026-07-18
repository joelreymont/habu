\ maki/db/diff-runner.f - the differential runner CORE (MODEL-CAD-V2-PLAN.md §
\ "Automatic differential verification", plan:3787-3796; dot
\ habu-v2-differential-runner-13359019).
\
\ CONCERN: over a sealed DIFFSUITE:suite, execute cases deterministically, compare
\ subject output vs reference output under the suite's DECLARED comparison domain +
\ tolerance, keep a hanging/dying subject DISTINCT from a numeric mismatch, minimize a
\ discrepancy WITHOUT replacing the original, and emit either SUCCESS evidence
\ (subject/suite/environment keyed, via the EVIDENCE registry) or a lossless FAILURE
\ diagnostic (the DIAG counterexample). The DifferentialSuite VALUE (maki/db/diff-suite.f),
\ the evidence identity registry (maki/db/evidence.f), and the structured Diagnostic IR
\ (maki/db/diagnostic.f) are CONSUMED here, never forked.
\
\ ---- ADAPTER INTERFACE (the runner core is execution-mechanism-agnostic) --------------
\ SUBJECT-RUN / REFERENCE-RUN are typed defer execution vectors (docs/forth.md "Execution
\ vectors are typed defer words"); a concrete adapter is installed with a typed quotation
\ through SUBJECT! / REFERENCE!. A case is identified by a scalar PARAMETER n (which is also
\ the index into DIFFSUITE:CASE-ID, so the case INPUT is content-addressed by
\ CASE-ID(suite,n)); the adapter consumes n and returns:
\   SUBJECT-RUN   ( n -- run-result )   produced v  (clean scalar output) | faulted (crash/hang/nonzero exit)
\   REFERENCE-RUN ( n -- ref-result )   value v     (reference scalar)      | skip (reference unavailable)
\ The SHIPPED spawn-isolated subject adapter and the external-process PyTorch reference
\ adapter live in maki/db/diff-runner-spawn.f (one concern per file: pure logic here,
\ process/fs isolation there). CLASSIFY-OUTCOME is the spawn taxonomy boundary: it maps a
\ lib/process.f `outcome` (exited/signaled/timeout) to produced-eligibility, so a hanging
\ (timeout, SIGKILL-reaped) or dying (signaled / nonzero exit) subject is FAULTED here, and
\ a clean exit(0) is the only produced-eligible outcome - the grader's EVN-DEVICE-FAULT vs
\ WRONG precedent (maki/eval/device.f). A faulted subject is NEVER compared, so it can never
\ be graded a wrong value.
\
\ ---- COMPARISON: composes NPOL:dom + the suite's u64 tolerance ------------------------
\ CLOSE? reads the suite's declared comparison domain (DIFFSUITE:COMPARE-DOM@) and tolerance
\ (DIFFSUITE:TOLERANCE@): `exact` licenses NO approximation so it requires bit-equal scalars;
\ every approximate domain bounds |subj-ref| by the tolerance (the diff-suite SEAL gate
\ already forbids an exact domain with nonzero tolerance and an approximate domain with zero
\ tolerance). This is the scalar (integer/bit) comparator; the tensor forward suite's
\ elementwise float comparator (the ort-ref pattern) is the first deferral.
\
\ ---- MINIMIZER: shrink a discrepancy to its minimal counterexample, ORIGINAL preserved -
\ MINIMIZE(suite,p) returns the least parameter in [0,p] that still MISMATCHES - the minimal
\ counterexample. It is a PURE function of (suite,p), so replaying the minimizer is
\ deterministic and the minimized case both runs identically. The original failing case p is
\ untouched; the minimized case is a SEPARATE content-addressed artifact CASE-ID(suite,p') !=
\ CASE-ID(suite,p). The suite's minimizer descriptor (a content key, unreadable as a runtime
\ strategy) RECORDS which minimizer identity produced the counterexample; the runner
\ implements the canonical least-counterexample shrink.
\
\ ---- EMISSION -------------------------------------------------------------------------
\ EMIT-EVIDENCE keys success evidence by the descriptor subject-key || suite-digest ||
\ environment-key, interned through EVIDENCE:REGISTER: flipping the subject, the suite (any
\ digest-covered field), or the environment yields a DISTINCT evidence identity, and an equal
\ triple interns to ONE id. EMIT-COUNTEREXAMPLE lowers a discrepancy to a DIAG:diagnostic
\ (class numeric, the failing subject as owner, a replay reproduction command, the reference
\ value as expected and the subject value as observed) - lossless on the DIAG wire.
\
\ FIRST-SLICE RUN-LOG: a bounded per-run array of LOG-CAP content-addressed case keys +
\ verdicts (the maki/db first-slice convention). maki -> habu only; diff-runner owns
\ -5395..-5398.

require lib/prelude.f
require lib/string.f
require lib/fmt.f
require lib/process.f               \ outcome sum: the spawn-classification boundary
require maki/cad-kinds.f
require maki/numpolicy.f            \ NPOL:dom lattice (comparison domain)
require maki/producer.f            \ CAD-KIND:producer-id (subject key)
require maki/config.f              \ CAD-KIND:config-id (environment key)
require maki/db/diff-suite.f       \ DIFFSUITE:suite value (consumed, not forked)
require maki/db/evidence.f         \ EVIDENCE:REGISTER / EQUAL? (success evidence identity)
require maki/db/diagnostic.f       \ DIAG:diagnostic counterexample (lossless failure IR)

-5395 constant E-DIFFRUN-CAP       \ run-log ring / evidence-descriptor scratch over capacity
-5396 constant E-DIFFRUN-RANGE     \ a case index / log slot outside its bound (fail-closed)
-5397 constant E-DIFFRUN-BUF       \ a fixed output buffer smaller than the required width
-5398 constant E-DIFFRUN-MISMATCH  \ the differential-discrepancy diagnostic code (a counterexample exists)

package DIFFRUN
public

\ ---- the runner's typed results (custom sums; never a value+flag sentinel) -------------
\ A subject run is a clean scalar output or a fault (crash/hang/nonzero exit).
SUMTYPE run-result 0
   VARIANT produced n ;VARIANT
   VARIANT faulted ;VARIANT
;SUMTYPE

\ A reference run is a scalar value or an unavailable (off-device / no PyTorch) skip.
SUMTYPE ref-result 0
   VARIANT value n ;VARIANT
   VARIANT skip ;VARIANT
;SUMTYPE

\ Per-case verdict: fault is its OWN member and is never a wrong value.
SUMTYPE case-verdict 0
   VARIANT agree ;VARIANT
   VARIANT mismatch ;VARIANT
   VARIANT subject-fault ;VARIANT
   VARIANT reference-skip ;VARIANT
;SUMTYPE

\ Whole-run verdict (deterministic first-failure order).
SUMTYPE run-verdict 0
   VARIANT verified ;VARIANT
   VARIANT falsified ;VARIANT
   VARIANT subject-faulted ;VARIANT
   VARIANT skipped ;VARIANT
;SUMTYPE

private

\ ---- readable wrappers over the generated constructor spellings -------------------------
: >PRODUCED ( n -- run-result )       DIFFRUN-RUN--RESULT:PRODUCED ;
: >FAULTED ( -- run-result )          DIFFRUN-RUN--RESULT:FAULTED ;
: >VALUE ( n -- ref-result )          DIFFRUN-REF--RESULT:VALUE ;
: >SKIP ( -- ref-result )             DIFFRUN-REF--RESULT:SKIP ;
: >AGREE ( -- case-verdict )          DIFFRUN-CASE--VERDICT:AGREE ;
: >MISMATCH ( -- case-verdict )       DIFFRUN-CASE--VERDICT:MISMATCH ;
: >SUBJECT-FAULT ( -- case-verdict )  DIFFRUN-CASE--VERDICT:SUBJECT-FAULT ;
: >REFERENCE-SKIP ( -- case-verdict ) DIFFRUN-CASE--VERDICT:REFERENCE-SKIP ;
: >VERIFIED ( -- run-verdict )        DIFFRUN-RUN--VERDICT:VERIFIED ;
: >FALSIFIED ( -- run-verdict )       DIFFRUN-RUN--VERDICT:FALSIFIED ;
: >SUBJECT-FAULTED ( -- run-verdict ) DIFFRUN-RUN--VERDICT:SUBJECT-FAULTED ;
: >SKIPPED ( -- run-verdict )         DIFFRUN-RUN--VERDICT:SKIPPED ;

\ ---- ordinal projections (stable small ints for scalar test assertions) ----------------
public
: CASE-VERDICT>N ( case-verdict -- n )
   MATCH case-verdict
      agree          OF 0 ENDOF
      mismatch       OF 1 ENDOF
      subject-fault  OF 2 ENDOF
      reference-skip OF 3 ENDOF
   ;MATCH ;

: RUN-VERDICT>N ( run-verdict -- n )
   MATCH run-verdict
      verified        OF 0 ENDOF
      falsified       OF 1 ENDOF
      subject-faulted OF 2 ENDOF
      skipped         OF 3 ENDOF
   ;MATCH ;

: RUN-RESULT>N ( run-result -- n )         \ 0 produced (value dropped), 1 faulted
   MATCH run-result
      produced OF drop 0 ENDOF
      faulted  OF 1 ENDOF
   ;MATCH ;

: RUN-RESULT-VALUE ( run-result -- n )     \ the produced scalar, or -1 on a fault
   MATCH run-result
      produced OF ENDOF
      faulted  OF -1 ENDOF
   ;MATCH ;

: REF-RESULT>N ( ref-result -- n )         \ 0 value (dropped), 1 skip
   MATCH ref-result
      value OF drop 0 ENDOF
      skip  OF 1 ENDOF
   ;MATCH ;

private

\ ---- adapter execution vectors (installed with typed quotations) -----------------------
defer SUBJECT-RUN ( n -- run-result )
defer REFERENCE-RUN ( n -- ref-result )

public
: SUBJECT! ( [ n -- run-result ] -- )    is SUBJECT-RUN ;
: REFERENCE! ( [ n -- ref-result ] -- )  is REFERENCE-RUN ;

\ ---- spawn taxonomy boundary: a process outcome -> produced-eligibility -----------------
\ Only a clean exit(0) is produced-eligible; timeout (hung, SIGKILL-reaped), signal death,
\ and a nonzero exit are all faults. Keeps a hanging/dying subject out of the compare path.
: CLASSIFY-OUTCOME ( outcome -- bool )
   MATCH outcome
      exited   OF 0 = ENDOF
      signaled OF drop false ENDOF
      timeout  OF false ENDOF
   ;MATCH ;

private

\ ---- scalar comparison under the suite's declared domain + tolerance --------------------
: ABS-N ( n -- n )   dup 0 < if negate then ;

public
: CLOSE? ( n n NPOL:dom n -- bool ) {: subj:n ref:n dom:NPOL:dom tol:n :}
   dom NPOL-DOM:EXACT NPOL-DOM:EQ if
      subj ref =
   else
      subj ref - ABS-N tol <=
   then ;

private

\ ---- per-case classification: fault dominates; then skip; then compare ------------------
: RESOLVE-REF ( DIFFSUITE:suite n n -- case-verdict ) {: h:DIFFSUITE:suite n:n subj:n :}
   n REFERENCE-RUN MATCH ref-result
      value OF {: ref:n :}
         subj ref  h DIFFSUITE:COMPARE-DOM@  h DIFFSUITE:TOLERANCE@  CLOSE?
         if >AGREE else >MISMATCH then
      ENDOF
      skip OF >REFERENCE-SKIP ENDOF
   ;MATCH ;

public
: CASE-VERDICT ( DIFFSUITE:suite n -- case-verdict ) {: h:DIFFSUITE:suite n:n :}
   n SUBJECT-RUN MATCH run-result
      produced OF {: subj:n :}  h n subj RESOLVE-REF ENDOF
      faulted  OF >SUBJECT-FAULT ENDOF
   ;MATCH ;

\ FAILS? is the minimizer / run predicate: a case is a DISCREPANCY iff it MISMATCHES (a
\ fault or a skip is not a discrepancy to minimize).
: FAILS? ( DIFFSUITE:suite n -- bool )   CASE-VERDICT CASE-VERDICT>N 1 = ;

\ ---- minimizer: least parameter in [0,p] that still mismatches (original preserved) -----
: MINIMIZE ( DIFFSUITE:suite n -- n ) {: h:DIFFSUITE:suite p:n :}
   0 begin dup p < while
      dup {: k:n :}
      h k FAILS? if drop k exit then
      1+
   repeat drop p ;

private

\ ---- bounded first-slice run-log (content-addressed input keys + verdicts) --------------
64 constant LOG-CAP                \ cases per run (first-slice bounded ring)
32 constant CIDW                   \ case-id width (SHA-256 content address of the input)
create LOG-KEY LOG-CAP CIDW * allot
create LOG-VERDICT LOG-CAP cells allot
variable LOG-N
variable AGREE-N variable MISMATCH-N variable FAULT-N variable SKIP-N
variable FF-N                      \ first-failing case index (-1 if none)
variable FF-KIND                   \ first-failing verdict ordinal (1 mismatch / 2 fault)

: LOG-RESET ( -- )
   0 LOG-N !  0 AGREE-N !  0 MISMATCH-N !  0 FAULT-N !  0 SKIP-N !
   -1 FF-N !  0 FF-KIND ! ;

: TALLY ( n -- ) {: v:n :}
   v 0 = if 1 AGREE-N +! exit then
   v 1 = if 1 MISMATCH-N +! exit then
   v 2 = if 1 FAULT-N +! exit then
   1 SKIP-N +! ;

: FAILURE? ( n -- bool )   dup 1 = swap 2 = or ;   \ mismatch or subject-fault

: RECORD ( n n -- ) {: idx:n v:n :}
   v idx cells LOG-VERDICT + !
   v TALLY
   v FAILURE? FF-N @ 0 < and if idx FF-N !  v FF-KIND ! then ;

: RUN-SUMMARY ( -- run-verdict )
   FF-N @ 0 >= if
      FF-KIND @ 2 = if >SUBJECT-FAULTED else >FALSIFIED then
   else
      SKIP-N @ 0 > if >SKIPPED else >VERIFIED then
   then ;

public

\ RUN executes cases [0,cases), storing each content-addressed input key CASE-ID(suite,k)
\ and its verdict, and returns the whole-run verdict by deterministic first-failure order.
: RUN ( DIFFSUITE:suite n -- run-verdict ) {: h:DIFFSUITE:suite cases:n :}
   cases 0 < cases LOG-CAP > or if E-DIFFRUN-CAP throw then
   LOG-RESET
   0 begin dup cases < while
      dup {: k:n :}
      h k  k CIDW * LOG-KEY +  DIFFSUITE:CASE-ID
      k  h k CASE-VERDICT CASE-VERDICT>N  RECORD
      1+
   repeat drop
   cases LOG-N !
   RUN-SUMMARY ;

\ ---- run-log read surface --------------------------------------------------------------
: FIRST-FAIL-CASE ( -- n )     FF-N @ ;
: AGREE-COUNT ( -- n )         AGREE-N @ ;
: MISMATCH-COUNT ( -- n )      MISMATCH-N @ ;
: FAULT-COUNT ( -- n )         FAULT-N @ ;
: SKIP-COUNT ( -- n )          SKIP-N @ ;
: LOG-COUNT ( -- n )           LOG-N @ ;
: CASE-KEY ( n ptr u8 n -- n ) {: k:n out:ptr cap:n :}   \ copy the k-th content-addressed input key
   k 0 < k LOG-N @ >= or if E-DIFFRUN-RANGE throw then
   cap CIDW < if E-DIFFRUN-BUF throw then
   k CIDW * LOG-KEY +  out  CIDW BYTE-COPY
   CIDW ;
: LOG-VERDICT@ ( n -- n ) {: k:n :}
   k 0 < k LOG-N @ >= or if E-DIFFRUN-RANGE throw then
   k cells LOG-VERDICT + @ ;

private

\ ---- success evidence: subject/suite/environment keyed identity -------------------------
96 constant EVD-DESC-CAP           \ subject-key(32) || suite-digest(32) || environment-key(32)
create EVD-DESC EVD-DESC-CAP allot

public
\ EMIT-EVIDENCE interns evidence keyed by the failing-free run's subject, suite digest, and
\ environment: flipping ANY of the three changes the descriptor and mints a distinct id;
\ an equal triple interns to ONE evidence id (content-addressed).
: EMIT-EVIDENCE ( DIFFSUITE:suite CAD-KIND:config-id -- CAD-KIND:evidence-id )
   {: h:DIFFSUITE:suite env:CAD-KIND:config-id :}
   h DIFFSUITE:SUBJECT@  EVD-DESC             CIDW  PRODUCER:KEY>WIRE {: w1:n :}
   h  EVD-DESC CIDW +                         CIDW  DIFFSUITE:DIGEST-INTO {: w2:n :}
   env  EVD-DESC CIDW 2 * +                   CIDW  CONFIG:KEY>WIRE {: w3:n :}
   EVD-DESC  w1 w2 + w3 +  EVIDENCE:REGISTER ;

private

\ ---- counterexample diagnostic (lossless DIAG lowering) --------------------------------
: DR-INT$ ( ptr u8 n n -- ptr u8 n ) {: a:ptr u:n v:n :}   \ "prefix<v>" into the shared builder
   SB-RESET  a u SB-APPEND  v SB-INT  SB$ ;

public
\ EMIT-COUNTEREXAMPLE lowers a minimized discrepancy at case p (subject value subj, reference
\ value ref) into a DIAG diagnostic: class numeric, the failing subject as owner, a replay
\ reproduction command, ref as expected and subj as observed. DIAG:BUILD rejects a partial
\ artifact (no owner / no reproduction) with a typed build-result.
: EMIT-COUNTEREXAMPLE ( DIFFSUITE:suite n n n -- DIAG:build-result )
   {: h:DIFFSUITE:suite p:n subj:n ref:n :}
   DIAG:NEW
   E-DIFFRUN-MISMATCH DIAG:CODE
   DIAG-CLASS:NUMERIC DIAG:CLASS
   DIAG-SEVERITY:ERROR DIAG:SEVERITY
   DIAG-PHASE:VERIFY DIAG:PHASE
   h DIFFSUITE:SUBJECT@ DIAG:OWNER
   s" diffrun replay case " p DR-INT$ DIAG:REPRODUCTION
   s" case " p DR-INT$ DIAG:LOCATION
   s" ref=" ref DR-INT$ DIAG:EXPECTED+
   s" subj=" subj DR-INT$ DIAG:OBSERVED+
   DIAG:BUILD ;

;package
