\ run-verdict-test.f - focused coverage for the TR-VERDICT retry driver.
\
\ Manual-standalone gate (not a TEST:SUITE member): run directly with
\   bin/hb --load lib/prelude.f lib/string.f lib/fmt.f lib/test.f \
\          test/perf-verdict.f test/run-verdict.f test/run-verdict-test.f
\
\ The driver's attempt seam (TR-VERDICT:MEASURE) is bound to a FAKE that returns
\ attempts from an injected per-attempt timing table, so the frozen retry rule
\ (initial pass -> 1 attempt; initial marginal -> exactly 2 more; 2-of-3; hard /
\ inadmissible / SHA-mismatch fail closed; never more than 3) is asserted with
\ NO real gate. The machine-line channel PA-EMIT/PA-PARSE is round-tripped on a
\ canned blob. All attempt (layout value) handling lives inside fixture words.

require lib/prelude.f
require lib/string.f
require lib/fmt.f
require lib/test.f
require test/perf-verdict.f
require test/run-verdict.f

package TR-VERDICT-TEST

10 constant FLD-N                         \ fields per injected attempt row
create INJ 3 FLD-N * cells allot          \ rows for attempts 1..3, field-major

variable RES
variable CNT

: ROW ( n -- ptr a ) {: n:n :}            \ base cell address of attempt-n row (n 1..3)
   INJ n 1- FLD-N * cells + ;

: CELL@ ( ptr a n -- a )
   cells + @ ;

: CELL! ( a ptr a n -- )
   cells + ! ;

\ Write one attempt row (booleans passed as -1/0 cells).
\ fields: e b pr po sh co ct fr em ca attempt
: INJ! ( n n n n n n n n n n n -- ) {: n:n :}
   n ROW
   \ typed-local-lint: allow-bare-local - base is a raw ptr a cell buffer.
   {: base :}
   {: e:n b:n pr:n po:n sh:n co:n ct:n fr:n em:n ca:n :}
   e  base 0 CELL!   b  base 1 CELL!   pr base 2 CELL!   po base 3 CELL!
   sh base 4 CELL!   co base 5 CELL!   ct base 6 CELL!   fr base 7 CELL!
   em base 8 CELL!   ca base 9 CELL! ;

\ The fake seam: build attempt n from its injected row.
\ Provenance is injected consistent (base=budget, pct=100, cold=false) so the
\ reproducibility gate is neutral and the injected timings drive the verdict.
: FAKE-MEASURE ( n -- PERF-VERDICT:att ) {: n:n :}
   n ROW
   \ typed-local-lint: allow-bare-local - base is a raw ptr a cell buffer.
   {: base :}
   base 0 CELL@  base 1 CELL@  base 2 CELL@  base 3 CELL@  base 4 CELL@
   base 5 CELL@ 0<>  base 6 CELL@ 0<>  base 7 CELL@ 0<>  base 8 CELL@ 0<>  base 9 CELL@ 0<>
   base 1 CELL@ 100 false
   PERF-VERDICT:ATTEMPT ;

: INSTALL ( -- )
   [: FAKE-MEASURE ;] is TR-VERDICT:MEASURE ;

\ Good-evidence defaults for one attempt at (elapsed, sha).
: GOOD! ( n n n -- ) {: n:n :}            \ elapsed sha attempt
   {: e:n sh:n :}
   e 1000 1000 1000 sh -1 -1 -1 -1 -1 n INJ! ;

: DO-RUN ( -- )
   TR-VERDICT:RUN RES !
   TR-VERDICT:ATTEMPTS@ CNT ! ;

\ Whole-trio driver over three elapseds, one shared SHA.
: TRI-RUN ( n n n -- ) {: e1:n e2:n e3:n :}   \ e1 e2 e3
   e1 42 1 GOOD!   e2 42 2 GOOD!   e3 42 3 GOOD!
   DO-RUN ;

: RES? ( -- bool )
   RES @ 0<> ;

\ ---- machine-line round trip (PA-LINE$ builder <-> PA-PARSE) ----------------
: GOOD-LINE ( -- ptr u8 n )                        \ built by the shared emit formatter
   900 1000 1000 1000 42 true true true 1000 100 false TR-VERDICT:PA-LINE$ ;

: LINE-MISSING ( -- ptr u8 n )                     \ budget field absent, plus surrounding noise
   S\" noise line\nperf-attempt\telapsed=900\tpre=1000\tpost=1000\tsha=42\tcorrect=1\tcontrol=1\tcache=1\ntail\n" ;

: PARSE-PASS? ( -- bool )
   GOOD-LINE true TR-VERDICT:PA-PARSE PERF-VERDICT:PASS? ;

: PARSE-ADM? ( -- bool )
   GOOD-LINE true TR-VERDICT:PA-PARSE PERF-VERDICT:ADMISSIBLE? ;

: PARSE-EMPTY-ADM? ( -- bool )                     \ parent supplies empty=false
   GOOD-LINE false TR-VERDICT:PA-PARSE PERF-VERDICT:ADMISSIBLE? ;

: PARSE-MISSING-HARD? ( -- bool )                  \ missing evidence -> fail-closed att
   LINE-MISSING true TR-VERDICT:PA-PARSE PERF-VERDICT:HARD-FAIL? ;

: PARSE-MISSING-ADM? ( -- bool )
   LINE-MISSING true TR-VERDICT:PA-PARSE PERF-VERDICT:ADMISSIBLE? ;

\ ===========================================================================
T-RESET
INSTALL

\ (1) initial pass -> exactly one attempt.
900 1000 1000 TRI-RUN
RES? TTRUE
CNT @ 1 T=

\ (2) initial hard fail -> one attempt, fail closed.
1200 900 900 TRI-RUN
RES? TFALSE
CNT @ 1 T=

\ (3) initial marginal + two passes -> pass, exactly three attempts.
1050 900 900 TRI-RUN
RES? TTRUE
CNT @ 3 T=

\ (4) initial marginal + one lucky pass + one marginal -> fail, three attempts.
1050 900 1080 TRI-RUN
RES? TFALSE
CNT @ 3 T=

\ (5) initial marginal, all three pass -> pass, three attempts.
1050 950 990 TRI-RUN
RES? TTRUE
CNT @ 3 T=

\ (6) initial marginal but a later hard fail -> fail closed, three attempts.
1050 900 1200 TRI-RUN
RES? TFALSE
CNT @ 3 T=

\ (7) marginal attempt 1, SHA mismatch across the trio -> fail closed.
1050 42 1 GOOD!   900 99 2 GOOD!   900 42 3 GOOD!
DO-RUN
RES? TFALSE
CNT @ 3 T=

\ (8) calibration drift on attempt 1 -> inadmissible, one attempt.
1050 1000 1000 1300 42 -1 -1 -1 -1 -1 1 INJ!   \ pre=1000 post=1300 -> 30% drift, marginal band
900 42 2 GOOD!   900 42 3 GOOD!
DO-RUN
RES? TFALSE
CNT @ 1 T=

\ (9) correctness missing on a pass-band attempt 1 -> inadmissible, one attempt;
\     correctness is a SEPARATE field (rendered f, admission gated).
900 1000 1000 1000 42 0 -1 -1 -1 -1 1 INJ!       \ correct=false
900 42 2 GOOD!   900 42 3 GOOD!
DO-RUN
RES? TFALSE
CNT @ 1 T=

\ (10) marginal attempt 1, reused / nonempty attempt-2 root (empty=false) -> fail closed.
1050 42 1 GOOD!
900 1000 1000 1000 42 -1 -1 -1 0 -1 2 INJ!       \ empty=false on attempt 2
900 42 3 GOOD!
DO-RUN
RES? TFALSE
CNT @ 3 T=

\ (11) machine-line round trip.
PARSE-PASS? TTRUE
PARSE-ADM? TTRUE
PARSE-EMPTY-ADM? TFALSE                            \ parent empty=false -> inadmissible
PARSE-MISSING-HARD? TTRUE
PARSE-MISSING-ADM? TFALSE

T-REPORT
s" run-verdict-test: ok" type cr

;package
