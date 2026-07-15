\ perf-verdict-test.f - focused coverage for the PERF-VERDICT policy package.
\
\ Manual-standalone gate (not a TEST:SUITE member, so suite-coverage-lint does
\ not route it): run directly with
\   bin/hb --load lib/prelude.f lib/string.f lib/fmt.f lib/test.f \
\          test/perf-verdict.f test/perf-verdict-test.f
\ Every acceptance fixture from dot habu-define-robust-gate-2f4b3e7b is an
\ executed assertion below. All attempt (layout value) construction lives inside
\ these fixture words; the top level only ever sees narrow bool/n/string results.

require lib/prelude.f
require lib/string.f
require lib/fmt.f
require lib/test.f
require test/perf-verdict.f

package PERF-VERDICT-TEST

\ ---- band fixtures (only elapsed/budget matter; evidence is fixed-good) -----
: BP ( n n -- bool ) {: e:n b:n :}                       \ pass-band predicate at (e,b)
   e b 1000 1000 42 true true true true true PERF-VERDICT:ATTEMPT PERF-VERDICT:PASS? ;

: BM ( n n -- bool ) {: e:n b:n :}                       \ marginal-band predicate at (e,b)
   e b 1000 1000 42 true true true true true PERF-VERDICT:ATTEMPT PERF-VERDICT:MARGINAL? ;

: BH ( n n -- bool ) {: e:n b:n :}                       \ hard-fail-band predicate at (e,b)
   e b 1000 1000 42 true true true true true PERF-VERDICT:ATTEMPT PERF-VERDICT:HARD-FAIL? ;

\ ---- calibration-stability fixture (only pre/post matter) -------------------
: CS ( n n -- bool ) {: pr:n po:n :}                     \ CAL-STABLE? at (pre,post)
   900 1000 pr po 42 true true true true true PERF-VERDICT:ATTEMPT PERF-VERDICT:CAL-STABLE? ;

\ ---- full-attempt fixtures over every field --------------------------------
: ADM ( n n n n n bool bool bool bool bool -- bool )
   PERF-VERDICT:ATTEMPT PERF-VERDICT:ADMISSIBLE? ;

: CORR ( n n n n n bool bool bool bool bool -- bool )
   PERF-VERDICT:ATTEMPT PERF-VERDICT:CORRECT? ;

: INIT ( n n n n n bool bool bool bool bool -- bool )
   PERF-VERDICT:ATTEMPT PERF-VERDICT:INITIAL-PASS? ;

: HF ( n n n n n bool bool bool bool bool -- bool )
   PERF-VERDICT:ATTEMPT PERF-VERDICT:HARD-FAIL? ;

\ ---- 2-of-3 aggregate fixtures ---------------------------------------------
\ three attempts with fixed-good evidence and one shared SHA, varying elapsed.
: TRI ( n n n -- bool ) {: e1:n e2:n e3:n :}
   e1 1000 1000 1000 42 true true true true true PERF-VERDICT:ATTEMPT
   e2 1000 1000 1000 42 true true true true true PERF-VERDICT:ATTEMPT
   e3 1000 1000 1000 42 true true true true true PERF-VERDICT:ATTEMPT
   PERF-VERDICT:TWO-OF-THREE? ;

\ three pass-band attempts, but the middle one carries a different SHA.
: TRI-SHA ( -- bool )
   900 1000 1000 1000 42 true true true true true PERF-VERDICT:ATTEMPT
   900 1000 1000 1000 99 true true true true true PERF-VERDICT:ATTEMPT
   900 1000 1000 1000 42 true true true true true PERF-VERDICT:ATTEMPT
   PERF-VERDICT:TWO-OF-THREE? ;

\ three pass-band attempts, but the middle one has a nonempty (reused) root.
: TRI-INADM ( -- bool )
   900 1000 1000 1000 42 true true true true true PERF-VERDICT:ATTEMPT
   900 1000 1000 1000 42 true true true false true PERF-VERDICT:ATTEMPT
   900 1000 1000 1000 42 true true true true true PERF-VERDICT:ATTEMPT
   PERF-VERDICT:TWO-OF-THREE? ;

\ ---- named-throw construction fixtures (fail loud on malformed inputs) ------
: BUDGET-CODE ( -- n )
   [: 1000 0 1000 1000 42 true true true true true PERF-VERDICT:ATTEMPT PERF-VERDICT:PASS? drop ;] catch ;

: ELAPSED-CODE ( -- n )
   [: -1 1000 1000 1000 42 true true true true true PERF-VERDICT:ATTEMPT PERF-VERDICT:PASS? drop ;] catch ;

: PRE-CODE ( -- n )
   [: 900 1000 0 1000 42 true true true true true PERF-VERDICT:ATTEMPT PERF-VERDICT:PASS? drop ;] catch ;

\ ---- deterministic ATTEMPT-LINE rows ---------------------------------------
: LINE-PASS ( -- ptr u8 n )
   900 1000 1000 1000 42 true true true true true PERF-VERDICT:ATTEMPT PERF-VERDICT:ATTEMPT-LINE ;

: LINE-FAIL ( -- ptr u8 n )
   1200 1000 1000 1300 0 false true true false true PERF-VERDICT:ATTEMPT PERF-VERDICT:ATTEMPT-LINE ;

\ ===========================================================================
T-RESET

\ (1) exact band thresholds at 100% and 110% (budget 1000: 1000, 1100 exact).
1000 1000 BP TTRUE            \ 100% is pass-band
1000 1000 BM TFALSE
1001 1000 BP TFALSE           \ one ms past 100% is not pass
1001 1000 BM TTRUE           \ ...it is marginal
1100 1000 BM TTRUE           \ 110% exactly is marginal
1100 1000 BH TFALSE
1101 1000 BH TTRUE           \ one ms past 110% is hard fail
1101 1000 BM TFALSE
\ integer-floor edge: 1005*110/100 = 1105 (floored); boundary is marginal.
1105 1005 BM TTRUE
1105 1005 BH TFALSE
1106 1005 BH TTRUE

\ (2) stable pass, single attempt, needs no retry.
900 1000 1000 1000 42 true true true true true INIT TTRUE

\ (3) noisy marginal + two passes -> pass.
1050 1000 900 TRI TTRUE

\ (4) one lucky pass + two marginal regressions -> fail.
900 1050 1080 TRI TFALSE

\ ...all three pass-band -> pass.
900 950 990 TRI TTRUE

\ (5) hard fail -> immediate fail (perf), even as a lone attempt or inside a trio.
1200 1000 1000 1000 42 true true true true true INIT TFALSE
1200 1000 BH TTRUE
900 1000 1200 TRI TFALSE     \ one hard fail in the trio fails closed

\ (6) calibration drift >10% -> attempt invalid -> fails closed.
1000 1100 CS TTRUE           \ exactly 10% is still stable
1000 1101 CS TFALSE          \ just past 10% is unstable
1000 900 CS TTRUE            \ -10% exactly is still stable
1000 899 CS TFALSE
900 1000 1000 1300 42 true true true true true ADM TFALSE   \ 30% drift -> inadmissible
900 1000 1000 1300 42 true true true true true INIT TFALSE  \ pass-band but drift -> not initial pass

\ (7) reused / nonempty attempt root -> fail closed.
900 1000 1000 1000 42 true true false true true ADM TFALSE  \ fresh=false (reused root)
900 1000 1000 1000 42 true true true false true ADM TFALSE  \ empty=false (nonempty root)

\ (8) unexpected within-attempt cache counters -> fail closed.
900 1000 1000 1000 42 true true true true false ADM TFALSE  \ cache=false

\ (9) missing correctness / control / SHA evidence -> fail closed.
900 1000 1000 1000 42 false true true true true ADM TFALSE  \ correctness missing
900 1000 1000 1000 42 true false true true true ADM TFALSE  \ control missing
900 1000 1000 1000 0 true true true true true ADM TFALSE    \ SHA missing (0)
\ ...and a fully-good attempt is admissible (positive control).
900 1000 1000 1000 42 true true true true true ADM TTRUE

\ correctness and performance are SEPARATE fields:
900 1000 1000 1000 42 true true true true true CORR TTRUE   \ correct+control -> correctness pass
900 1000 1000 1000 42 false true true true true CORR TFALSE \ correctness missing
900 1000 1000 1000 42 true false true true true CORR TFALSE \ control missing
1200 1000 1000 1000 42 true true true true true CORR TTRUE  \ hard-fail perf but correctness passes
1200 1000 1000 1000 42 true true true true true HF TTRUE    \ ...same inputs, perf hard fail

\ (10) SHA mismatch across attempts -> fail closed; matching SHA -> admitted.
TRI-SHA TFALSE
TRI-INADM TFALSE             \ one reused root in the trio also fails closed

\ (11) deterministic ATTEMPT-LINE rows (byte-exact).
LINE-PASS s" attempt e=900 b=1000 band=pass pre=1000 post=1000 stable=t sha=42 correct=t control=t fresh=t empty=t cache=t admissible=t" T$=
LINE-FAIL s" attempt e=1200 b=1000 band=hard pre=1000 post=1300 stable=f sha=0 correct=f control=t fresh=t empty=f cache=t admissible=f" T$=

\ named-throw construction guards.
BUDGET-CODE E-PV-BUDGET T=
ELAPSED-CODE E-PV-RANGE T=
PRE-CODE E-PV-RANGE T=

T-REPORT
s" perf-verdict-test: ok" type cr

;package
