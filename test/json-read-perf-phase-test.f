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
\   - A steady bracket is admissible however SLOW the box reads, and a drifted
\     one is refused however fast it reads. Speed alone no longer refuses
\     anything: the verdicts are ratios, so a uniformly slower machine cancels
\     out, and only an unsteady one is inadmissible.
\   - The load sampler parses a real /proc/loadavg, reports n/a rather than a
\     number on a host without one, and rejects malformed content instead of
\     inventing a sample from it.
\   - Spawning real CPU-bound neighbours raises the recorded runnable-process
\     count, so the sample on the evidence line tracks actual machine load.
\   - The phase does not touch the SHARED performance factor. That factor is
\     scaled into the engine runtime-slice ratchet, both stdlib tail ratchets
\     and the MATCH compile bench; this phase used to overwrite it from its own
\     spin, and two brackets at different scripted speeds now leave it exactly
\     where they found it.
\   - The phase runs once per gate process. A second START is refused with
\     E-JRPP-REPEAT, and a REFUSED start does not consume that one turn.
\
\ Run: bin/hb --load test/json-read-perf-phase-test.f

require lib/test.f
require test/json-read-perf-phase.f

package JSON-READ-PERF-PHASE
private

\ Sized by the work a worker actually does, then load-scaled. The bound that
\ matters is ATTEMPT-MAX, not two: EXHAUST-WORKER scripts a box that never goes
\ quiet, so it runs the phase's full retry budget - THREE complete measurements,
\ every workload and the reference, before it may report. The old constant said
\ "two full brackets" and was already short of that; it survived only because
\ each measurement used to be cheaper. Load-scaled on top, because a fixed
\ wall-clock budget in a process-spawning suite is the documented flake class
\ (lib/test/budget.f) and these workers are forks. A worker that reaches this is
\ genuinely stuck, not merely unlucky.
: WORK-TIMEOUT-MS ( -- n )            \ ATTEMPT-MAX full measurements, with margin
   90000 T-BUDGET-MS ;

\ No host profile is pinned here any more, and its absence is load-bearing. The
\ phase used to divide its spin reading by a per-profile reference that is 0
\ until a profile is applied, so these cases had to install one. Nothing in the
\ phase reads a host reference now - the verdicts are ratios - so a case that
\ needed one would be evidence the calibration had crept back in.
1000 constant PROBE-BASE              \ a round base, so a scripted reading reads straight off

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

\ What the bracket does and does not decide. Pure arithmetic on two readings, so
\ it needs no measurement and no host profile - which is itself the point: the
\ admissibility rule no longer consults a per-host reference at all.
\
\ The middle case is the one that changed. A stable bracket on a THREE TIMES
\ SLOWER box used to be refused, because the absolute budgets were nanosecond
\ counts recorded on a fast machine and the compensation factor had saturated at
\ its clamp. Verdicts are ratios now, so a uniformly slower box is simply a
\ slower box: both the workloads and the reference slow together and the ratio
\ is unchanged. Restore any speed-based refusal here and this case fails.
100 constant BRACKET-BASE             \ an arbitrary steady reading; only the ratios matter

: CASE-BRACKET ( -- )
   s" a steady bracket is admissible" T-LABEL
   BRACKET-BASE BRACKET-BASE ADMISSIBLE? TTRUE
   s" a steady bracket on a three times slower box is admissible too" T-LABEL
   BRACKET-BASE 3 *  BRACKET-BASE 3 *  ADMISSIBLE? TTRUE
   s" a bracket that drifted is refused however fast the box is" T-LABEL
   BRACKET-BASE  BRACKET-BASE 2 *  ADMISSIBLE? TFALSE ;

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

\ ---- hostile input: the sample ends exactly at a mapping boundary ----------
\ Poisoning the byte after the input cannot fail. Forth `and` evaluates both
\ operands, so an out-of-bounds read is DISCARDED by and-with-false and no value
\ placed there can change a verdict. The only honest hostile input is one whose
\ next byte cannot be read at all, so this places the sample against a real
\ boundary: two 64K regions are mapped and the second is handed back to the
\ kernel, leaving the first ending at an unmapped address. 64K is a multiple of
\ every ARM64 page size, so that split is always page-aligned.
\
\ The sample is written flush against the boundary and passed to the REAL parser
\ entry points. The input is deliberately TRUNCATED - fewer fields than
\ LOAD-RUN-FIELD asks for - so the scan must walk to the very end, which is the
\ case that reads one past. A scan that dereferences before testing its bounds
\ faults here; the guarded scan stops.
MEM-64K constant FENCE-SPAN

: FENCE-BASE ( -- ptr u8 )
   FENCE-SPAN 2 * MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop ;

: FENCE-ARM ( ptr u8 -- ) {: base:ptr :}
   base FENCE-SPAN BYTE+ FENCE-SPAN munmap drop ;

: FENCE-FREE ( ptr u8 -- ) {: base:ptr :}
   base FENCE-SPAN munmap drop ;

\ Copy the sample so its LAST byte is the last readable byte of the mapping.
: FENCE-PUT ( ptr u8 ptr u8 n -- ptr u8 n ) {: base:ptr src:ptr u:n :}
   base FENCE-SPAN u - BYTE+ {: dst:ptr :}
   src dst u BYTE-COPY
   dst u ;

: CASE-END-OF-MAPPING ( -- )
   FENCE-BASE {: base:ptr :}
   base FENCE-ARM
   s" 0.31 2.59" {: sa:ptr su:n :}
   base sa su FENCE-PUT {: fa:ptr fu:n :}
   s" a truncated sample flush against an unmapped page yields no runnable count" T-LABEL
   fa fu LOAD-RUNNABLE LOAD-NONE T=
   s" the one-minute average still reads from that same sample" T-LABEL
   fa fu LOAD-AVG-X100 31 T=
   base FENCE-FREE ;

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

\ ---- the spin reaches NOTHING but the bracket --------------------------------
\ The inverse of the regression this file used to assert, and it guards a real
\ boundary rather than an internal detail. ATTEMPT used to install its spin
\ reading as the process-wide performance factor
\ (`pre FACTOR TEST-BUDGET:PERF-SET`). That factor is SHARED: the engine
\ runtime-slice ratchet, both stdlib tail ratchets and the MATCH compile bench
\ all scale their own budgets by it. This phase has no business moving it, and
\ now does not - its verdicts are ratios and need no factor at all.
\
\ So: pin the factor to a known value, run two brackets whose scripted spin
\ readings differ by a factor of two, and require the shared factor to be
\ exactly what it was. Reinstate any PERF-SET call in ATTEMPT and the second
\ reading moves it and this worker dies.
100 constant PINNED-PCT               \ the uncalibrated floor: a value ATTEMPT must not move

: FACTOR-UNTOUCHED-WORKER ( -- )
   PINNED-PCT TEST-BUDGET:PERF-SET
   SCRIPT-INSTALL!
   PROBE-BASE  PROBE-BASE  PROBE-BASE 2 *  PROBE-BASE 2 *  SCRIPT!
   ATTEMPT drop                       \ bracket one: the box is at the scripted speed
   PROBE-BASE TEST-BUDGET:PERF-MS PROBE-BASE EXPECT-EQ
   ATTEMPT drop                       \ bracket two: the box reads half as fast
   PROBE-BASE TEST-BUDGET:PERF-MS PROBE-BASE EXPECT-EQ ;

: CASE-WORKERS ( -- )
   GT-POOL-FIND-FREE {: retry:idx :}
   s" jrpp contended retry" WORK-TIMEOUT-MS retry [: RETRY-WORKER ;] GT-POOL-START-FORK-SLOT
   GT-POOL-FIND-FREE {: exhaust:idx :}
   s" jrpp contended exhaust" WORK-TIMEOUT-MS exhaust [: EXHAUST-WORKER ;] GT-POOL-START-FORK-SLOT
   GT-POOL-FIND-FREE {: budget:idx :}
   s" jrpp shared factor untouched" WORK-TIMEOUT-MS budget [: FACTOR-UNTOUCHED-WORKER ;] GT-POOL-START-FORK-SLOT
   GT-POOL-DRAIN-SOFT
   s" a contended bracket is re-measured and a clean one after it is accepted" T-LABEL
   retry GT-POOL-OK? TTRUE
   s" a box that never goes quiet exits with the phase's own status" T-LABEL
   exhaust GT-POOL-EXITED-PTR @ TTRUE
   exhaust GT-POOL-CODE-PTR @ CONTENDED-RC T=
   s" that status is not the one a failed benchmark leaves behind" T-LABEL
   CONTENDED-RC 1 <> TTRUE
   s" the phase leaves the shared performance factor alone" T-LABEL
   budget GT-POOL-OK? TTRUE ;

: MAIN ( -- )
   T-RESET
   s" json-read-perf-phase-test" GT-START
   LOAD-NEIGHBOURS 2 + GT-POOL-SLOTS!
   GT-POOL-RESET
   CASE-ADMISSION
   CASE-BRACKET
   CASE-LOAD-PARSE
   CASE-END-OF-MAPPING
   CASE-LOAD-SPAWN
   CASE-WORKERS
   GT-CLEANUP
   T-REPORT ;

MAIN

;package

s" json-read-perf-phase-test: ok" type cr
