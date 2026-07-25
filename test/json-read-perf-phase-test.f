\ json-read-perf-phase-test.f - focused coverage for the quiescent ratchet phase.
\
\ The phase's two claims are admission and the re-measure bound, and both are
\ cheap to falsify, so this file drives them directly instead of re-running the
\ ratchets. It reopens package JSON-READ-PERF-PHASE for the same reason
\ lib/json-read-perf-contract-test.f reopens the provider: the words under test
\ are the real private ones, not copies.
\
\ What it proves:
\   - START run inside a REAL gate-pool fork worker refuses with E-JRPP-CHILD,
\     judged by that worker's exit status, not by anything the parent reports.
\   - That refusal is caused by the fork child's own slot stamp: a worker that
\     clears its stamp is no longer refused as a child, and is refused as a busy
\     caller instead, with the other named code.
\   - That same unstamped worker, holding a real undrained nested pool worker, is
\     refused with E-JRPP-BUSY, and the same admission word admits it again once
\     the nested worker is reaped.
\   - A calibration bracket that drifts is re-measured EXACTLY once: the scripted
\     probe holds four readings and refuses a fifth, so a second re-measure would
\     surface as a capacity throw instead of the phase's own code.
\   - Two drifted brackets end in E-JRPP-DRIFT rather than another attempt.
\   - The spin the phase measures actually decides the budget: two brackets that
\     differ only in their scripted spin reading must produce two different
\     budgets, so measuring the spin and then dropping it cannot pass.
\   - The phase runs once per gate process. A second START is refused with
\     E-JRPP-REPEAT, and a REFUSED start does not consume that one turn.
\
\ Run: bin/hb --load test/json-read-perf-phase-test.f

require lib/test.f
require test/json-read-perf-phase.f

package JSON-READ-PERF-PHASE
private

\ Load-scaled, not fixed: a fixed wall-clock budget in a process-spawning suite
\ is the documented flake class (lib/test/budget.f), and these workers are forks.
: WORK-TIMEOUT-MS ( -- n )            \ headroom for two full brackets
   30000 T-BUDGET-MS ;

\ The per-profile spin reference the phase now calibrates against is 0 until a
\ host profile is applied, which would make every factor the 100% floor and hide
\ a broken budget wiring. Pin the committed Spark profile so the reference is a
\ known nonzero number and two different spin readings must produce two
\ different budgets.
: PROFILE-PIN! ( -- )
   TR-PROFILE-DGX-SPARK-10X2 TR-PROFILE-ID ! ;

1000 constant PROBE-BASE              \ a round base, so the factor reads straight off the budget

\ ---- scripted calibration probe -------------------------------------------
\ Four readings is exactly two brackets. Reading past the script throws, so
\ "the phase re-measured a second time" cannot be mistaken for a drift verdict.
4 constant SCRIPT-MAX
create SCRIPT SCRIPT-MAX cells allot
variable SCRIPT-N
variable SCRIPT-I

: SCRIPT-RESET ( -- )
   0 SCRIPT-N !
   0 SCRIPT-I ! ;

: SCRIPT+ ( n -- ) {: ms:n :}
   SCRIPT-N @ SCRIPT-MAX >= if E-TBL-BOUNDS throw then
   ms SCRIPT SCRIPT-N @ cells + !
   SCRIPT-N @ 1+ SCRIPT-N ! ;

: SCRIPT-MS ( -- n )
   SCRIPT-I @ SCRIPT-N @ >= if E-TBL-BOUNDS throw then
   SCRIPT SCRIPT-I @ cells + @
   SCRIPT-I @ 1+ SCRIPT-I ! ;

: SCRIPT-INSTALL! ( -- )
   [: SCRIPT-MS ;] is PROBE-MS ;

\ pre post pre post, in the order the two brackets read them
: SCRIPT! ( n n n n -- ) {: a:n b:n c:n d:n :}
   SCRIPT-RESET
   a SCRIPT+ b SCRIPT+ c SCRIPT+ d SCRIPT+ ;

\ ---- fork-worker bodies ---------------------------------------------------
\ Both bodies run in a real GT-POOL-START-FORK child, so their verdict is the
\ worker's exit status: returning exits 0, any throw dies nonzero through
\ GT-POOL-FORK-THROW.
: EXPECT-EQ ( n n -- ) {: got:n want:n :}
   got want = if exit then
   s" json-read-perf-phase-test: expected " type want . cr
   s" json-read-perf-phase-test: got " type got . cr
   E-JRPP-MISMATCH throw ;

\ A refused START must leave the phase's one turn unclaimed, which is what pins
\ CLAIM behind ADMIT: if the latch were claimed first, this worker would have
\ burned the turn on a call that measured nothing.
: CHILD-STAMPED ( -- )                \ an ordinary worker: the stamp must refuse START
   [: START ;] catch E-JRPP-CHILD EXPECT-EQ
   RAN @ 0 EXPECT-EQ ;

: IDLE-WORKER ( -- )
   s" json-read-perf-phase idle worker" type cr ;

\ Hostile control AND the busy-caller case in one worker. It clears its own slot
\ stamp, so if START refused on anything but the stamp it would still report
\ E-JRPP-CHILD and the case above would prove nothing. With the stamp gone it
\ starts a REAL nested pool worker and leaves it unreaped, which is exactly the
\ "caller is not quiescent" state, then drains and checks that the same
\ admission word admits the now-idle caller.
\
\ It has to be a worker rather than a top-level case because this file itself
\ runs inside a gate fork worker: at top level in the gate the stamp is already
\ set and every admission would stop at E-JRPP-CHILD.
: CHILD-UNSTAMPED ( -- )
   0 GS-CHILD-U !
   1 GT-POOL-SLOTS!
   GT-POOL-RESET
   s" jrpp nested idle worker" WORK-TIMEOUT-MS [: IDLE-WORKER ;] GT-POOL-START-FORK
   [: START ;] catch E-JRPP-BUSY EXPECT-EQ
   GT-POOL-DRAIN-SOFT
   GT-POOL-LIVE @ 0 EXPECT-EQ
   [: ADMIT ;] catch 0 EXPECT-EQ
   [: CLAIM ;] catch 0 EXPECT-EQ      \ the admitted caller takes the phase's one turn
   [: CLAIM ;] catch E-JRPP-REPEAT EXPECT-EQ
   \ With the turn already taken, an otherwise admissible START must refuse. This
   \ is what pins START to the latch: drop the CLAIM call from START and this
   \ worker runs the real measurement instead of throwing.
   [: START ;] catch E-JRPP-REPEAT EXPECT-EQ ;

\ ---- pool-backed cases ----------------------------------------------------
\ GT-POOL-OK?, not the raw exit code: a worker the pool timed out records code 0,
\ so only "exited, and exited zero" may count as the worker agreeing.
: FORK-CASE ( ptr u8 n [ -- ] -- bool )  \ label, body -> did the worker exit clean?
   \ typed-local-lint: allow-bare-local - q keeps the forked worker quotation effect.
   {: label:ptr labelu:n q :}
   GT-POOL-FIND-FREE {: idx:idx :}
   label labelu WORK-TIMEOUT-MS idx q GT-POOL-START-FORK-SLOT
   GT-POOL-DRAIN-SOFT
   idx GT-POOL-OK? ;

: CASE-ADMISSION ( -- )
   s" a fork worker's own slot stamp refuses the phase" T-LABEL
   s" jrpp stamped child" [: CHILD-STAMPED ;] FORK-CASE TTRUE
   s" without that stamp the refusal is the busy one, and drains away" T-LABEL
   s" jrpp unstamped child" [: CHILD-UNSTAMPED ;] FORK-CASE TTRUE ;

\ ---- re-measure bound -----------------------------------------------------
\ Each bracket runs the real MEASURE, so the two cases run as concurrent pool
\ workers rather than back to back, and each one's verdict is its exit status.
: DRIFT-ONCE-WORKER ( -- )
   SCRIPT-INSTALL!
   100 200 100 101 SCRIPT!
   STABLE-ONCE                        \ a stable re-measure must be accepted
   SCRIPT-I @ 4 EXPECT-EQ ;           \ exactly one re-measure, no more

: DRIFT-TWICE-WORKER ( -- )
   SCRIPT-INSTALL!
   100 200 100 200 SCRIPT!
   [: STABLE-ONCE ;] catch E-JRPP-DRIFT EXPECT-EQ
   SCRIPT-I @ 4 EXPECT-EQ ;           \ no third bracket was attempted

\ ---- the pre-spin actually reaches the budget --------------------------------
\ The regression the review demanded. ATTEMPT measures a spin and is supposed to
\ turn it into this run's budget factor; before the fix it measured the spin and
\ discarded it, because the reference it divided by was zero off macOS and every
\ factor pinned to the 100% floor. Two brackets whose only difference is the
\ scripted spin reading must therefore produce two different budgets: delete the
\ `pre FACTOR TEST-BUDGET:PERF-SET` wiring, or divide by the wrong reference
\ again, and both readings collapse to the same number and this worker dies.
: BUDGET-WORKER ( -- )
   PROFILE-PIN!
   TR-CAL-REF-MS {: ref:n :}          \ the committed performance-core spin time
   SCRIPT-INSTALL!
   ref  ref  ref 2 *  ref 2 *  SCRIPT!
   ATTEMPT drop                       \ bracket one: the box is at the reference speed
   PROBE-BASE TEST-BUDGET:PERF-MS PROBE-BASE EXPECT-EQ
   ATTEMPT drop                       \ bracket two: the box reads half as fast
   PROBE-BASE TEST-BUDGET:PERF-MS PROBE-BASE 2 * EXPECT-EQ ;

: CASE-WORKERS ( -- )
   GT-POOL-FIND-FREE {: once:idx :}
   s" jrpp drift once" WORK-TIMEOUT-MS once [: DRIFT-ONCE-WORKER ;] GT-POOL-START-FORK-SLOT
   GT-POOL-FIND-FREE {: twice:idx :}
   s" jrpp drift twice" WORK-TIMEOUT-MS twice [: DRIFT-TWICE-WORKER ;] GT-POOL-START-FORK-SLOT
   GT-POOL-FIND-FREE {: budget:idx :}
   s" jrpp budget wiring" WORK-TIMEOUT-MS budget [: BUDGET-WORKER ;] GT-POOL-START-FORK-SLOT
   GT-POOL-DRAIN-SOFT
   s" one drifted bracket is re-measured once and accepted when stable" T-LABEL
   once GT-POOL-OK? TTRUE
   s" two drifted brackets end in the phase's own drift code" T-LABEL
   twice GT-POOL-OK? TTRUE
   s" the measured spin decides the budget, it is not measured and dropped" T-LABEL
   budget GT-POOL-OK? TTRUE ;

: MAIN ( -- )
   T-RESET
   s" json-read-perf-phase-test" GT-START
   3 GT-POOL-SLOTS!
   GT-POOL-RESET
   CASE-ADMISSION
   CASE-WORKERS
   GT-CLEANUP
   T-REPORT ;

MAIN

;package

s" json-read-perf-phase-test: ok" type cr
