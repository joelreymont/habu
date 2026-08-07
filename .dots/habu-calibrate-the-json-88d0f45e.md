---
title: The json-read ratchets have no real headroom left
status: active
priority: 2
issue-type: task
created-at: "2026-08-07T15:11:40.133039+02:00"
---

CORRECTION FIRST. The original text of this dot said the ratchet budgets are scaled by HB_CAL_PCT measured once at gate start, and asked for them to be scaled by the probe taken at the measurement instead. THAT IS WRONG, and anyone acting on it would be reimplementing what is already there. test/json-read-perf-phase.f ATTEMPT already does `PROBE-MS ... pre FACTOR TEST-BUDGET:PERF-SET` INSIDE the forked worker, immediately before JSON-READ-PERF-TEST:MEASURE, and its header says so explicitly ("the driver measured once at gate start ... while this measures in the worker that is about to run the workloads, on the core it is about to run them on"). Arithmetic confirms it: budget = base x HEADROOM-PCT/100 x CAL/100, and the implied CAL of every observed run matches that run's own `pre` reading against TEST:CAL-REF-MS, not the driver's factor. There is nothing to move.

THE REAL PROBLEM is that two of the six ratchets have no headroom left, and the calibration factor is all that is holding them up.

MEASURED (macOS arm64, one idle box, 2026-08-07). Zero-calibration budget is base x 1.10: raw string 109841024 -> 120825126, escape-heavy 189988832 -> 208987715. Judged statistic is fastest-of-five.

  run                     probe  CAL   raw fastest      /zero-cal   esc fastest      /zero-cal
  master 3782b295         105    110%  130520083        1.080       223645833        1.070
  branch, run A           107    112%  135259042        1.120       233611291        1.118
  branch, run B (peer)    104    109%  131796958        1.091       ~226.9M          1.086  RED on raw

Every run is 7 to 12 percent ABOVE the budget the recorded baselines allow at a calibration factor of 100. These rows pass only because this box's calibration spin reads 104..107 ms against the committed TEST:CAL-REF-MS of 95, which inflates every budget by about 10 percent. ON A BOX THAT MATCHED THE REFERENCE, raw string and escape-heavy ARE RED ON MASTER. The true headroom is roughly zero, so a two-millisecond wobble in the spin probe decides the verdict, which is why the same tree goes green and red on consecutive runs.

SECOND FINDING: the spin probe is not a valid normaliser for these workloads. CAL-SPIN is a register-only multiply loop; the four string and search workloads are memory bound. Standalone runs on the same box, same tree: probe 131 ms (factor 138%) gave escape-heavy 291803167 against budget 286313169, FAIL; probe 118 ms (factor 124%) gave raw string 152218792 against budget 149823156, FAIL. The probe moved 1.24x to 1.4x while the workloads moved about 2.2x. And in the peer's gate run the probe read FASTER than master's (104 vs 105) while the workload ran 1.0 percent SLOWER. A normaliser that moves in the wrong direction is not correcting anything.

WHAT THIS IS NOT. It is not caused by the branch that found it (habu-47-registered-suites). That branch adds no JSON code; it lengthens the gate from 1:47 to 3:09 by scheduling the proof and tail slices, which shifts the probe by a couple of milliseconds - enough to flip a verdict whose margin is 0.05 percent. Moving the phase earlier in the run would only pick a different point on the same uncontrolled axis and would buy a lucky green, not a fix.

THREE CANDIDATE FIXES, and the first thing to do is decide WHICH, because they have different owners:

  (a) A REAL REGRESSION. The string-decode path may genuinely be ~8 percent slower than when the baselines were recorded. The baselines name parents 83fae24d6628 and aa2a169469ad, and NEITHER RESOLVES in this repo any more, so the comparison tree has to be recovered another way before this can be settled. Candidate commits touching lib/json-read.f: bc6b49080b49 "Restore explicit JSON reader throughput", ddb4f44c8a18 "Harden explicit JSON reader", 82fa49c1f160 "Make JSON reader state explicit" (all 2026-07-22), d32daa5bbd0e "Harden JSON object key lookup". If one of these cost 8 percent, the ratchet has been doing its job and the calibration factor has been hiding the result.

  (b) STALE BASELINES. If (a) comes back clean, the baselines describe a machine this tree no longer runs on and must be re-recorded WITH the measurement and the reason written down. That is a deliberate recorded decision. It is NOT something anyone does to unblock a merge, and it must not be done before (a) is answered.

  (c) THE NORMALISER, which needs replacing whatever (a) and (b) say. A wall-clock ratchet on a shared box is only meaningful as a RATIO to something measured in the same round. Add a frozen reference workload to lib/json-read-perf-test.f that touches the same memory profile as the decode loops but calls NO code under test - a fixed strided scan or copy over a comparable buffer - time it in the same interleaved rounds, and judge fastest(workload)/fastest(reference) against the recorded ratio. That is immune to both machine state and where in the run the measurement lands. It must NOT be calibrated against the JSON workloads themselves: a self-referential normaliser grows the budget exactly when the parser regresses and blinds the ratchet completely. Falsify it by slowing one decode word by ten percent and checking the row still fails, and by running the phase at gate start and at gate end and checking the verdict does not move.

DO NOT: raise HEADROOM-PCT, raise SAMPLE-N to fish for a faster fastest, or loosen CAL-SPIN:DRIFT-OK?. None of those change what is being measured.

Claim: agent=jsonratio workspace=.jj-ws/habu-calibrate-the-json-88d0f45e

FIXED 2026-08-07 (agent=jsonratio) by option (c), with two corrections to the
option itself that only measurement could have found.

(a) IS UNANSWERABLE, PROVEN TWO WAYS, and is recorded as such rather than
glossed. The baseline parents 83fae24d6628 and aa2a169469ad do not resolve. The
four candidate commits named above DO resolve, but their lib/json-read.f no
longer compiles under the current checker, which has since tightened
(`in push: at 'c!' expected: u8 ptr u8 actual: n ptr n`). So there is no tree to
re-measure and no source that can be timed on today's engine. The regression
question against the old numbers cannot be settled by any available route.

(b) The baselines were re-recorded, but only after (c) was working, and with the
basis written beside them in docs/gate.md.

(c) TWO CORRECTIONS TO THE OPTION AS WRITTEN:

  1. "A fixed byte-scan/copy over a committed buffer is the shape" is WRONG for
     this host, measured. A flat scan made the verdicts WORSE than no reference
     at all on four of six rows (escape-heavy 4.4% -> 9.2% spread). This box is
     8 performance + 4 efficiency cores and each loop SHAPE has its own P-to-E
     penalty, so a reference of the wrong shape compounds core placement instead
     of cancelling it - the register-spin defect in a new costume. The reference
     now copies the reader's silhouette (per-item call, byte-at-a-time
     classify-and-copy, teardown), which roughly halved five of six rows.

  2. BOTH TERMS MUST BE MEASURED THE SAME WAY, and this was found by the
     falsification, not by reasoning. Timing the reference as the fastest of ten
     short sub-runs while timing each workload as one long run gave the
     denominator quiet gaps the numerator never got: a HEALTHY tree then red 5
     of 6 rows under load. Every slot is now the fastest of SLOT-CHUNKS sub-runs
     of a tenth of its own work. Symmetry is the correctness condition.

FALSIFICATION (all on the real MEASURE/REPORT path):
  - injected slowdown in the decode loop (lib/json-read.f UNESC-ALL): red the
    four decode rows 5/5 and left the two numeric rows alone 0/5 - a targeted
    result, not a blanket one;
  - healthy tree under eight extra busy loops (load 25): 0/5 red on all six;
  - uniform scaling of every slot by 2 and by 97 moves no verdict, asserted in
    lib/json-read-perf-contract-test.f against real ratio arithmetic.

DELETED: the spin normalisation for these six ratchets, and with it the
saturation half of admissibility (there is no compensation left to saturate, and
a merely slow box is now fine). The shared factor is UNTOUCHED - the engine
runtime slice, both stdlib tail ratchets and the MATCH compile bench still read
it - and a new phase-test case pins that this phase no longer writes it.

KNOWN LIMIT, recorded rather than hidden: a ~10% regression is inside this
host's measured noise and will not fail a row; the class reliably separated
starts near 15%. The previous numbers had no headroom at all.

STILL OPEN: the basis was taken across load 11-25 with the spin at 144-179 ms,
never in a quiet window (a foreign build held 6-8 of 12 cores for the whole
session), so the gate's own 104-107 ms range is bracketed by argument and by the
scale-invariance test, not by direct measurement. A quiet-box re-record would
tighten HEADROOM-PCT and is worth doing when a window exists.
