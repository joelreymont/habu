\ maki/db/diff-runner-tensor.f - the TENSOR forward differential suite (the ort-ref
\ elementwise-float leg the scalar runner core deferred; maki/db/diff-runner.f dot
\ habu-v2-differential-runner-13359019).
\
\ CONCERN: run a tensor forward suite deterministically, comparing the subject's OUTPUT
\ TENSOR against the reference's elementwise under the suite's DECLARED comparison domain +
\ tolerance (the maki/onnx/ort-ref-test.f ORF-CLOSE? pattern: `f- fabs tol f<=`, now folded
\ over a float ARRAY), keep a hanging/dying subject DISTINCT from a numeric mismatch, and
\ minimize a discrepancy WITHOUT replacing the original. It REOPENS package DIFFRUN and
\ REUSES the scalar core whole: the run-result / ref-result / case-verdict / run-verdict
\ sums (the `n` payload is now the produced ELEMENT COUNT; the float data lives in the
\ SUBJ-T / REF-T compare buffers, the maki EX-OUT fixed-buffer convention), CLASSIFY-OUTCOME
\ (fault dominates), the bounded run-log (LOG-RESET / RECORD / RUN-SUMMARY), EMIT-EVIDENCE
\ (subject/suite/environment keyed - suite-level, so the tensor suite reuses it unchanged),
\ and the DR-INT$ diagnostic builder. Only the COMPARATOR changes: scalar equality/abs
\ becomes elementwise float tolerance.
\
\ ---- ADAPTER INTERFACE (tensor variant of the scalar defer vectors) ------------------
\ T-SUBJECT-RUN / T-REFERENCE-RUN are typed defer execution vectors installed with a typed
\ quotation through T-SUBJECT! / T-REFERENCE!. A case is a scalar PARAMETER n (the CASE-ID
\ index). The adapter fills the shared compare buffer and returns the element count:
\   T-SUBJECT-RUN   ( n -- run-result )   produced <len>  (SUBJ-T[0..len) filled) | faulted
\   T-REFERENCE-RUN ( n -- ref-result )   value <len>     (REF-T[0..len) filled)  | skip
\ The isolated spawn subject + subject-source injection live in maki/db/diff-runner-inject.f
\ (one concern per file: pure comparison here, process/injection there).
\
\ ---- COMPARISON DOMAIN/TOLERANCE over float arrays -----------------------------------
\ T-CLOSE? reads the suite's NPOL:dom (DIFFSUITE:COMPARE-DOM@) and u64 tolerance
\ (DIFFSUITE:TOLERANCE@): `exact` licenses NO approximation, so it requires elementwise
\ EXACT equality (a zero float bound); every approximate domain bounds |subj-ref| by the
\ tolerance elementwise. The u64 tolerance is read as a FIXED-POINT bound in units of 1e-9
\ (TOL-SCALE): a suite tolerance of 10000 is an absolute elementwise bound of 1e-5 - the
\ ort-ref f32-vs-f64 rounding floor (maki/onnx/ort-ref-test.f ORF-TOL). The diff-suite SEAL
\ gate already forbids exact-with-nonzero-tolerance and approximate-with-zero-tolerance, so
\ the two arms are exhaustive. maki -> habu only; tensor leg owns -5411.

require lib/prelude.f               \ f<= (float bound comparison)
require lib/float.f                 \ float ops (f- fabs, s>f)
require lib/fmt.f                   \ SB-FIX (counterexample float render)
require maki/array.f                \ T-GET / T-SET (float tensor cell access)
require maki/db/diff-runner.f       \ package DIFFRUN: sums, wrappers, run-log, EMIT-EVIDENCE, DR-INT$

-5411 constant E-DTEN-CAP           \ a tensor length exceeds the fixed compare-buffer capacity

package DIFFRUN
private

64 constant TCAP                    \ max elements per compared tensor (fixed compare buffers)
1000000000 constant TOL-SCALE       \ suite u64 tolerance is a fixed-point bound in units of 1e-9
6 constant TFIX-DIGITS              \ counterexample float render precision
create SUBJ-T TCAP cells allot      \ subject output tensor (filled by the subject adapter)
create REF-T  TCAP cells allot      \ reference output tensor (filled by the reference adapter)
create T-TOLF 1 cells allot         \ current elementwise float tolerance (bound)

\ ---- tolerance projection: u64 fixed-point (1e-9 units) -> float bound ---------------
: TOL>F ( n -- r )   s>f  TOL-SCALE s>f  f/ ;

\ ---- shared tolerance cell (reuses the proven float accessor, no float @ typing) -----
: T-TOLF@ ( -- r )   T-TOLF 0 T-GET ;
: T-TOLF! ( r -- )   T-TOLF 0 T-SET ;
: T-SET-TOL ( NPOL:dom n -- ) {: dom:NPOL:dom tol:n :}
   dom NPOL-DOM:EXACT NPOL-DOM:EQ if 0.0 else tol TOL>F then  T-TOLF! ;

\ ---- elementwise closeness (the ort-ref pattern folded over the array) ---------------
: T-ELT-CLOSE? ( n -- bool ) {: i:n :}
   SUBJ-T i T-GET  REF-T i T-GET  f- fabs  T-TOLF@  f<= ;

public

\ ---- shared compare-buffer write surface (adapters fill these) ------------------------
: T-LEN-OK ( n -- n )   dup 0 < over TCAP > or if E-DTEN-CAP throw then ;
: SUBJ-T! ( r n -- )   T-LEN-OK  SUBJ-T swap T-SET ;
: REF-T! ( r n -- )    T-LEN-OK  REF-T swap T-SET ;
: SUBJ-T@ ( n -- r )   SUBJ-T swap T-GET ;
: REF-T@ ( n -- r )    REF-T swap T-GET ;

\ T-CLOSE? is true iff every element of SUBJ-T[0..len) is within the suite's declared
\ domain+tolerance of REF-T[0..len) (exact -> a zero bound -> elementwise equality).
: T-CLOSE? ( n NPOL:dom n -- bool ) {: len:n dom:NPOL:dom tol:n :}
   dom tol T-SET-TOL
   0 begin dup len < while
      dup {: i:n :}  i T-ELT-CLOSE? 0= if drop false exit then  1+
   repeat drop true ;

private

\ ---- adapter execution vectors (installed with typed quotations) ----------------------
defer T-SUBJECT-RUN ( n -- run-result )
defer T-REFERENCE-RUN ( n -- ref-result )

public
: T-SUBJECT! ( [ n -- run-result ] -- )    is T-SUBJECT-RUN ;
: T-REFERENCE! ( [ n -- ref-result ] -- )  is T-REFERENCE-RUN ;

private

\ ---- per-case classification: fault dominates; then skip; then elementwise compare ----
\ A length disagreement is a shape mismatch (a genuine discrepancy), never a fault.
: T-RESOLVE-REF ( DIFFSUITE:suite n n -- case-verdict )
   {: h:DIFFSUITE:suite n:n slen:n :}
   n T-REFERENCE-RUN MATCH ref-result
      value OF {: rlen:n :}
         slen rlen = if
            slen  h DIFFSUITE:COMPARE-DOM@  h DIFFSUITE:TOLERANCE@  T-CLOSE?
            if >AGREE else >MISMATCH then
         else >MISMATCH then
      ENDOF
      skip OF >REFERENCE-SKIP ENDOF
   ;MATCH ;

public
: T-CASE-VERDICT ( DIFFSUITE:suite n -- case-verdict ) {: h:DIFFSUITE:suite n:n :}
   n T-SUBJECT-RUN MATCH run-result
      produced OF {: slen:n :}  h n slen T-RESOLVE-REF ENDOF
      faulted  OF >SUBJECT-FAULT ENDOF
   ;MATCH ;

\ FAILS? / MINIMIZE: a case is a DISCREPANCY iff it MISMATCHES; the minimizer is the least
\ parameter in [0,p] that still mismatches (a fault/skip is not a discrepancy to minimize).
: T-FAILS? ( DIFFSUITE:suite n -- bool )   T-CASE-VERDICT CASE-VERDICT>N 1 = ;

: T-MINIMIZE ( DIFFSUITE:suite n -- n ) {: h:DIFFSUITE:suite p:n :}
   0 begin dup p < while
      dup {: k:n :}
      h k T-FAILS? if drop k exit then
      1+
   repeat drop p ;

\ T-RUN executes cases [0,cases), logging each content-addressed input key and verdict
\ through the shared run-log, and returns the whole-run verdict by first-failure order.
: T-RUN ( DIFFSUITE:suite n -- run-verdict ) {: h:DIFFSUITE:suite cases:n :}
   cases 0 < cases LOG-CAP > or if E-DIFFRUN-CAP throw then
   LOG-RESET
   0 begin dup cases < while
      dup {: k:n :}
      h k  k CIDW * LOG-KEY +  DIFFSUITE:CASE-ID
      k  h k T-CASE-VERDICT CASE-VERDICT>N  RECORD
      1+
   repeat drop
   cases LOG-N !
   RUN-SUMMARY ;

\ ---- counterexample: first mismatching element, lossless DIAG lowering ---------------
: T-FIRST-DIFF ( n -- n ) {: len:n :}    \ first mismatching element index (T-TOLF preset), or len
   0 begin dup len < while
      dup {: i:n :}  i T-ELT-CLOSE? 0= if exit then  1+
   repeat ;

\ "prefix<r>" into the shared builder (float rendered at TFIX-DIGITS; r passed UNDER prefix).
: TFIX$ ( r ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   SB-RESET  a u SB-APPEND  TFIX-DIGITS FMT:SB-FIX  SB$ ;

\ T-EMIT-COUNTEREXAMPLE lowers a minimized tensor discrepancy at case p (subject SUBJ-T,
\ reference REF-T, len elements) into a DIAG diagnostic: class numeric, the failing subject
\ as owner, a replay reproduction, the first mismatching element index as the location, and
\ that element's ref value as expected / subj value as observed.
: T-EMIT-COUNTEREXAMPLE ( DIFFSUITE:suite n n -- DIAG:build-result )
   {: h:DIFFSUITE:suite p:n len:n :}
   h DIFFSUITE:COMPARE-DOM@  h DIFFSUITE:TOLERANCE@  T-SET-TOL
   len T-FIRST-DIFF {: idx:n :}
   DIAG:NEW
   E-DIFFRUN-MISMATCH DIAG:CODE
   DIAG-CLASS:NUMERIC DIAG:CLASS
   DIAG-SEVERITY:ERROR DIAG:SEVERITY
   DIAG-PHASE:VERIFY DIAG:PHASE
   h DIFFSUITE:SUBJECT@ DIAG:OWNER
   s" difftensor replay case " p DR-INT$ DIAG:REPRODUCTION
   s" elt " idx DR-INT$ DIAG:LOCATION
   idx REF-T@  s" ref=" TFIX$ DIAG:EXPECTED+
   idx SUBJ-T@ s" subj=" TFIX$ DIAG:OBSERVED+
   DIAG:BUILD ;

;package
