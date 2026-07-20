\ perf-verdict.f - PERF-VERDICT: a PURE full-gate performance-verdict policy.
\
\ This package judges NUMBERS it is handed. It does no I/O, spawns no process,
\ and reads no clock; the runner/evidence integration (dot
\ habu-wire-perf-verdict-runner-7f26769e) supplies the measured numbers and this
\ package decides pass/marginal/hard, calibration stability, admissibility, and
\ the 2-of-3 retry aggregate deterministically.
\
\ It implements the FROZEN verdict rule (parent dot habu-make-full-gate-7b4f9440,
\ 2026-07-13) exactly:
\   - Bracket every attempt with pre/post calibration; >10% calibration drift
\     makes the attempt INVALID.
\   - elapsed <= 100% of the calibrated budget = pass-band; >100%..<=110% =
\     marginal; >110% = hard fail.
\   - An initial pass needs no retry (INITIAL-PASS?).
\   - An initial marginal runs exactly two more attempts and passes only when at
\     least 2 of 3 are pass-band (TWO-OF-THREE?). Any hard fail or invalid/missing
\     evidence fails CLOSED.
\   - Every admitted attempt carries distinct-fresh + proven-empty cache roots,
\     the same under-test SHA, correctness evidence, control evidence, and
\     expected within-attempt cache counters matching their committed contract.
\   - Correctness and performance are SEPARATE verdict fields (CORRECT? vs the
\     band predicates).
\
\ Modeling notes (flagged where the frozen rule left a choice):
\   - The under-test SHA is modeled as one identity cell `n`; the integration
\     folds the 32-byte digest into a cell. sha=0 means "no SHA evidence" and
\     fails closed. TWO-OF-THREE? requires all three SHAs equal AND present.
\   - "distinct fresh roots proven empty" and "expected within-attempt cache
\     counters match their contract" reach this pure layer as already-decided
\     booleans (fresh, empty, cache); proving those facts is the integration's
\     job. The policy fails closed when any is false.
\   - Calibration drift is measured relative to the PRE calibration; the |post-pre|
\     band is <=10% inclusive (exactly 10% is stable, ">10%" is invalid).
\   - The 110% band edge uses integer floor (budget*110/100); the boundary value
\     is marginal, one ms past it is a hard fail.
\
\ Load after lib/prelude.f, lib/string.f, lib/fmt.f.

require lib/prelude.f
require lib/string.f
require lib/fmt.f

\ Named policy throw codes (unclaimed -4500..-4599 block; lib/errors.f owns up to
\ -4499). Kept global so the integration and tests can name them. Migrate into
\ lib/errors.f if this graduates out of test/.
-4500 constant E-PV-FIRST
-4599 constant E-PV-LAST
-4500 constant E-PV-BUDGET   \ calibrated budget must be positive
-4501 constant E-PV-RANGE    \ a measurement (elapsed/pre/post/sha) is out of range

package PERF-VERDICT

\ ---- private policy vocabulary --------------------------------------------
ENUM band pass marginal hard ;ENUM

110 constant PV-MARGIN-PCT    \ marginal band ceiling as a percent of budget
100 constant PV-HUNDRED       \ percent denominator
10 constant PV-DRIFT-PCT      \ max tolerated calibration drift, percent of pre
2 constant PV-PASS-N          \ pass-band attempts required out of three

public

\ The typed attempt record. It must be PUBLIC for the declaration surface to
\ generate PERF--VERDICT-ATT:MAKE / PERF--VERDICT-ATT:UNMAKE (a private product
\ generates nothing); external callers use the PERF-VERDICT:* API below, never
\ the raw layout. Fields are in MAKE/UNMAKE slot order (elapsed deepest).
PRODUCT att 0
   FIELD elapsed n
   FIELD budget n
   FIELD pre n
   FIELD post n
   FIELD sha n
   FIELD correct bool
   FIELD control bool
   FIELD fresh bool
   FIELD empty bool
   FIELD cache bool
;PRODUCT

private

: MARGIN-HI ( n -- n )                          \ budget -> integer 110%-of-budget edge
   PV-MARGIN-PCT * PV-HUNDRED / ;

: BAND-OF ( n n -- band ) {: e:n b:n :}          \ elapsed budget -> band
   e b <= if construct band pass exit then
   e b MARGIN-HI <= if construct band marginal exit then
   construct band hard ;

: IS-PASS? ( band -- bool )
   MATCH band pass OF true ENDOF marginal OF false ENDOF hard OF false ENDOF ;MATCH ;

: IS-MARGINAL? ( band -- bool )
   MATCH band pass OF false ENDOF marginal OF true ENDOF hard OF false ENDOF ;MATCH ;

: IS-HARD? ( band -- bool )
   MATCH band pass OF false ENDOF marginal OF false ENDOF hard OF true ENDOF ;MATCH ;

: BAND-NAME$ ( band -- ptr u8 n )
   MATCH band
      pass OF s" pass" ENDOF
      marginal OF s" marginal" ENDOF
      hard OF s" hard" ENDOF
   ;MATCH ;

: DRIFT-OK? ( n n -- bool ) {: pr:n po:n :}      \ pre post -> within +-10% of pre
   po pr - {: d:n :}
   d 0 < if d negate else d then {: ad:n :}
   ad PV-HUNDRED *  pr PV-DRIFT-PCT *  <= ;

: EVIDENCE-OK? ( n bool bool bool bool bool -- bool )   \ sha correct control fresh empty cache
   {: sh:n co:bool ct:bool fr:bool em:bool ca:bool :}
   sh 0 <>  co and  ct and  fr and  em and  ca and ;

: ADMIT? ( n n n bool bool bool bool bool -- bool )     \ pre post sha correct control fresh empty cache
   {: pr:n po:n sh:n co:bool ct:bool fr:bool em:bool ca:bool :}
   pr po DRIFT-OK?
   sh co ct fr em ca EVIDENCE-OK?
   and ;

\ ---- render helpers (append into the lib/string.f SB builder) --------------
: SB-TF ( bool -- )
   if s" t" else s" f" then SB-APPEND ;

: SB-U+ ( ptr u8 n n -- ) {: v:n :}              \ append "label" then decimal value
   SB-APPEND v FMT:SB-U ;

: SB-TF+ ( ptr u8 n bool -- ) {: v:bool :}       \ append "label" then t/f
   SB-APPEND v SB-TF ;

\ ---- one-consume reducers over the att bundle ------------------------------
\ Peel every field once; return band + calibration + evidence + sha as narrow
\ cells so the public predicates never destructure the layout value twice.
: ATT>SUM ( att -- band bool bool n )
   PERF--VERDICT-ATT:UNMAKE
   {: e:n b:n pr:n po:n sh:n co:bool ct:bool fr:bool em:bool ca:bool :}
   e b BAND-OF
   pr po DRIFT-OK?
   sh co ct fr em ca EVIDENCE-OK?
   sh ;

\ pass? hard? admissible? sha - the per-attempt shape the 2-of-3 aggregate folds.
: ATT>PHAS ( att -- bool bool bool n )
   ATT>SUM {: bd:band st:bool ev:bool sh:n :}
   bd IS-PASS?  bd IS-HARD?  st ev and  sh ;

public

\ Build a validated attempt record. Malformed CONSTRUCTION (nonpositive budget,
\ negative measurement) is a caller bug and throws a named code; the verdict
\ PREDICATES below never throw, they fail closed.
: ATTEMPT ( n n n n n bool bool bool bool bool -- att )
   {: e:n b:n pr:n po:n sh:n co:bool ct:bool fr:bool em:bool ca:bool :}
   b 0 <= if E-PV-BUDGET throw then
   e 0 < if E-PV-RANGE throw then
   pr 0 <= if E-PV-RANGE throw then
   po 0 <= if E-PV-RANGE throw then
   sh 0 < if E-PV-RANGE throw then
   e b pr po sh co ct fr em ca PERF--VERDICT-ATT:MAKE ;

: PASS? ( att -- bool )                          \ pure pass-band predicate
   ATT>SUM {: bd:band st:bool ev:bool sh:n :} bd IS-PASS? ;

: MARGINAL? ( att -- bool )                      \ pure marginal-band predicate
   ATT>SUM {: bd:band st:bool ev:bool sh:n :} bd IS-MARGINAL? ;

: HARD-FAIL? ( att -- bool )                     \ pure hard-fail-band predicate
   ATT>SUM {: bd:band st:bool ev:bool sh:n :} bd IS-HARD? ;

: CAL-STABLE? ( att -- bool )                    \ pre/post drift within +-10%
   ATT>SUM {: bd:band st:bool ev:bool sh:n :} st ;

: ADMISSIBLE? ( att -- bool )                    \ cal-stable AND all evidence present
   ATT>SUM {: bd:band st:bool ev:bool sh:n :} st ev and ;

: CORRECT? ( att -- bool )                       \ correctness field, SEPARATE from perf
   PERF--VERDICT-ATT:UNMAKE
   {: e:n b:n pr:n po:n sh:n co:bool ct:bool fr:bool em:bool ca:bool :}
   co ct and ;

: INITIAL-PASS? ( att -- bool )                  \ initial pass needs no retry
   ATT>SUM {: bd:band st:bool ev:bool sh:n :} bd IS-PASS? st and ev and ;

\ Retry aggregate over three attempts. Fails closed on any inadmissible attempt,
\ any hard fail, or a SHA mismatch; otherwise passes iff >=2 are pass-band.
: TWO-OF-THREE? ( att att att -- bool )
   ATT>PHAS {: p3:bool h3:bool a3:bool s3:n :}
   ATT>PHAS {: p2:bool h2:bool a2:bool s2:n :}
   ATT>PHAS {: p1:bool h1:bool a1:bool s1:n :}
   a1 a2 and a3 and 0= if false exit then
   h1 h2 or h3 or if false exit then
   s1 s2 = s1 s3 = and 0= if false exit then
   0 p1 if 1+ then p2 if 1+ then p3 if 1+ then
   PV-PASS-N >= ;

\ Deterministic per-attempt evidence row (bytes into the shared SB builder;
\ compare or copy before the next SB use).
: ATTEMPT-LINE ( att -- ptr u8 n )
   PERF--VERDICT-ATT:UNMAKE
   {: e:n b:n pr:n po:n sh:n co:bool ct:bool fr:bool em:bool ca:bool :}
   SB-RESET
   s" attempt e=" e SB-U+
   s"  b=" b SB-U+
   s"  band=" SB-APPEND e b BAND-OF BAND-NAME$ SB-APPEND
   s"  pre=" pr SB-U+
   s"  post=" po SB-U+
   s"  stable=" pr po DRIFT-OK? SB-TF+
   s"  sha=" sh SB-U+
   s"  correct=" co SB-TF+
   s"  control=" ct SB-TF+
   s"  fresh=" fr SB-TF+
   s"  empty=" em SB-TF+
   s"  cache=" ca SB-TF+
   s"  admissible=" pr po sh co ct fr em ca ADMIT? SB-TF+
   SB$ ;

;package
