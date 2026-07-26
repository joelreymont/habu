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
\   - A contended bracket is re-measured rather than judged, and a clean bracket
\     after it is accepted: the scripted probe proves exactly two brackets ran.
\   - A box that never goes quiet exhausts ATTEMPT-MAX brackets and leaves with
\     CONTENDED-RC. The verdict is that exact exit status, not merely "nonzero":
\     a benchmark that missed its budget dies 1 through the ordinary fork-throw
\     path, so the two outcomes are proven distinguishable rather than assumed.
\   - A UNIFORMLY contended box is refused too. Its bracket is perfectly stable,
\     so the drift rule alone would admit it; the saturation rule catches it,
\     which is the whole reason admissibility is not just DRIFT-OK?.
\   - The load sampler parses a real /proc/loadavg, reports n/a rather than a
\     number on a host without one, and rejects malformed content instead of
\     inventing a sample from it.
\   - Spawning real CPU-bound neighbours raises the recorded runnable-process
\     count, so the sample on the evidence line tracks actual machine load.
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
\ Eight readings is four brackets - one more than ATTEMPT-MAX allows. Reading
\ past the script throws, so "the phase measured one more time than its bound"
\ can never be mistaken for the phase refusing a contended box.
8 constant SCRIPT-MAX
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

\ ---- retry bound and the refusal ------------------------------------------
\ Each bracket runs the real MEASURE, so the cases run as concurrent pool
\ workers rather than back to back, and each one's verdict is its exit status.
: RETRY-WORKER ( -- )
   SCRIPT-INSTALL!
   100 200 100 101 SCRIPT!
   MEASURE-ADMISSIBLE                 \ a clean bracket after a contended one is accepted
   SCRIPT-I @ 4 EXPECT-EQ ;           \ exactly two brackets, no more

\ Every bracket drifts, so the phase must give up at its bound and leave with
\ CONTENDED-RC. It never returns, so the assertion is the exit status the parent
\ reads; a worker that instead fell through to REPORT would exit 0 or 1 here.
: EXHAUST-WORKER ( -- )
   SCRIPT-INSTALL!
   SCRIPT-RESET
   100 SCRIPT+ 200 SCRIPT+
   100 SCRIPT+ 200 SCRIPT+
   100 SCRIPT+ 200 SCRIPT+
   MEASURE-ADMISSIBLE ;

\ A box that is evenly loaded for the whole bracket does not drift at all: pre
\ and post agree, DRIFT-OK? is happy, and only saturation says the numbers are
\ worthless. Pure, so it needs no measurement.
: CASE-SATURATION ( -- )
   PROFILE-PIN!
   TR-CAL-REF-MS {: ref:n :}
   s" an idle bracket is admissible" T-LABEL
   ref ref ADMISSIBLE? TTRUE
   s" a stable bracket on a box past the compensation clamp is refused" T-LABEL
   ref 3 *  ref 3 *  ADMISSIBLE? TFALSE
   s" a drifted bracket is refused even below the clamp" T-LABEL
   ref  ref 2 *  ADMISSIBLE? TFALSE ;

\ ---- load sampling --------------------------------------------------------
\ Structural, not substring: each case hands the parser a byte buffer and reads
\ the two numbers back, including buffers built to look plausible but be wrong.
create LOAD-FIX 64 allot
variable LOAD-FIX-U

: LOAD-FIX! ( ptr u8 n -- ) {: a:ptr u:n :}
   a LOAD-FIX u BYTE-COPY
   u LOAD-FIX-U ! ;

: LOAD-FIX$ ( -- ptr u8 n )
   LOAD-FIX LOAD-FIX-U @ ;

: CASE-LOAD-PARSE ( -- )
   s" 0.31 2.59 2.82 4/702 2013010" LOAD-FIX!
   s" the one-minute average is read in hundredths" T-LABEL
   LOAD-FIX$ LOAD-AVG-X100 31 T=
   s" the runnable count is the numerator of the fourth field" T-LABEL
   LOAD-FIX$ LOAD-RUNNABLE 4 T=
   s" a two-digit whole part is not truncated" T-LABEL
   s" 12.07 2.59 2.82 17/702 9" LOAD-FIX!
   LOAD-FIX$ LOAD-AVG-X100 1207 T=
   LOAD-FIX$ LOAD-RUNNABLE 17 T=
   s" content with no decimal point yields no sample" T-LABEL
   s" garbage here" LOAD-FIX!
   LOAD-FIX$ LOAD-AVG-X100 LOAD-NONE T=
   s" a truncated line yields no runnable sample" T-LABEL
   s" 0.31 2.59" LOAD-FIX!
   LOAD-FIX$ LOAD-RUNNABLE LOAD-NONE T=
   s" a fourth field with no slash yields no runnable sample" T-LABEL
   s" 0.31 2.59 2.82 702 2013010" LOAD-FIX!
   LOAD-FIX$ LOAD-RUNNABLE LOAD-NONE T= ;

\ ---- real spawned load ----------------------------------------------------
\ The sampler is proved against REAL neighbours, not a fixture. It stays small
\ and short on purpose: this file runs as one slot of the gate's own pool, so a
\ leg that saturated every core to force an inadmissible verdict would corrupt
\ the timing of every other slot running beside it. The refusal logic is proved
\ deterministically by the scripted brackets above; what a real spawn adds, and
\ all it needs to add, is that the recorded number tracks the actual machine.
4 constant LOAD-NEIGHBOURS
6 constant SPIN-REPS                  \ about half a second of real CPU per neighbour:
                                      \ far longer than the spawn-then-sample window,
                                      \ short enough not to disturb the slots beside it

: SPIN-CHILD ( -- )
   SPIN-REPS 0 ?do
      T-BUDGET-CAL-ITERS T-BUDGET-CAL-SPIN drop
   loop ;

: SPAWN-NEIGHBOUR ( -- )
   GT-POOL-FIND-FREE {: idx:idx :}
   s" jrpp load neighbour" WORK-TIMEOUT-MS idx [: SPIN-CHILD ;] GT-POOL-START-FORK-SLOT ;

: CASE-LOAD-SPAWN ( -- )
   LOAD-NOW {: quiet-avg:n quiet-run:n :}
   LOAD-NEIGHBOURS 0 ?do SPAWN-NEIGHBOUR loop
   LOAD-NOW {: busy-avg:n busy-run:n :}
   GT-POOL-DRAIN-SOFT
   quiet-run LOAD-NONE = if
      s" a host with no load file reports n/a for both samples" T-LABEL
      busy-run LOAD-NONE T=
      exit
   then
   \ A lower bound justified by construction, NOT a comparison against the
   \ pre-spawn sample. LOAD-NEIGHBOURS CPU-bound children were just forked and
   \ are runnable, so the sample cannot read below that on any box. Comparing
   \ against the earlier reading looked stronger and was actually flaky: this
   \ file runs as one slot of the gate's own pool, where the ambient runnable
   \ count moves by more between the two samples than the neighbours add, so
   \ the count can legitimately fall while the neighbours are running.
   s" the runnable sample counts real CPU-bound neighbours" T-LABEL
   busy-run LOAD-NEIGHBOURS >= TTRUE ;

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
   GT-POOL-FIND-FREE {: retry:idx :}
   s" jrpp contended retry" WORK-TIMEOUT-MS retry [: RETRY-WORKER ;] GT-POOL-START-FORK-SLOT
   GT-POOL-FIND-FREE {: exhaust:idx :}
   s" jrpp contended exhaust" WORK-TIMEOUT-MS exhaust [: EXHAUST-WORKER ;] GT-POOL-START-FORK-SLOT
   GT-POOL-FIND-FREE {: budget:idx :}
   s" jrpp budget wiring" WORK-TIMEOUT-MS budget [: BUDGET-WORKER ;] GT-POOL-START-FORK-SLOT
   GT-POOL-DRAIN-SOFT
   s" a contended bracket is re-measured and a clean one after it is accepted" T-LABEL
   retry GT-POOL-OK? TTRUE
   s" a box that never goes quiet exits with the phase's own status" T-LABEL
   exhaust GT-POOL-EXITED-PTR @ TTRUE
   exhaust GT-POOL-CODE-PTR @ CONTENDED-RC T=
   s" that status is not the one a failed benchmark leaves behind" T-LABEL
   CONTENDED-RC 1 <> TTRUE
   s" the measured spin decides the budget, it is not measured and dropped" T-LABEL
   budget GT-POOL-OK? TTRUE ;

: MAIN ( -- )
   T-RESET
   s" json-read-perf-phase-test" GT-START
   LOAD-NEIGHBOURS 2 + GT-POOL-SLOTS!
   GT-POOL-RESET
   CASE-ADMISSION
   CASE-SATURATION
   CASE-LOAD-PARSE
   CASE-LOAD-SPAWN
   CASE-WORKERS
   GT-CLEANUP
   T-REPORT ;

MAIN

;package

s" json-read-perf-phase-test: ok" type cr
