---
title: Attribute the candidate-validation forge red
status: active
priority: 2
issue-type: task
created-at: "2026-08-06T17:36:40.365173+02:00"
---

Master 44aa8134: test/run.f red at 'baseline validation shared' inside the candidate-validation slice — 3 failures, the visible suspects the layout forge cases (layout-valid-walk-forge.f exit 70, layout-valid-desc-forge.f exit 70) judged against recorded baselines. Reproduced twice by the orchestrator in merge-gate with a FRESH fixpoint engine, so not stale-seed. Was green at c68209e1 this morning and in every lane's own gates through spillwire; landed since: thecut audit (comments/dots), openplain memo carry (elaborate.f — not in engine seed), insnrows SMULH/MADD/MSUB (asm.f + formal — IS engine source; its lane's gates were green on ITS tree, gates not re-run post-rebase). Also possibly the same event the noemit lane saw as text-foundation assert 741 at ba7935d1 (standalone green for the orchestrator — pool/composition sensitivity suspected). Attribute by running the slice at each landing since c68209e1 with a per-commit fixpoint engine; fix at the root; the recorded-baseline machinery's sensitivity to engine growth is the design question if that is the cause. Blocks every merge (zero-red bar). 2026-08-06.

Claim: agent=candfix workspace=.jj-ws/habu-attr-the-candidate

## Attribution result: no landing is responsible

Each commit run with its OWN fixpoint-refreshed engine, in its own workspace,
one at a time on an otherwise idle host:

| commit | landing | full test/run.f | lint-artifacts/fast |
| --- | --- | --- | --- |
| c68209e1 | last known green | exit 0, zero red | PASS |
| ba7935d1 | openplain memo carry | exit 0, zero red | PASS |
| 44aa8134 | insnrows (asm.f) | exit 0, zero red, twice | PASS |

Two premises in the report above are wrong, and they matter:

- There is no recorded-baseline table. `test/gate-validation-worker.f:36`
  runs `test/candidate-validation.f` twice — once with the candidate engine,
  once with the tree's `bin/hb` — and `CHECK-BASELINE` compares the two
  evidence streams byte for byte. Nothing is recorded, so nothing can go
  stale, and engine growth cannot move a pinned row. The forge cases' exit 70
  is their EXPECTED value (`test/candidate-validation.f:244-249`), not a
  divergence.
- Assertion 741 of `tools/lint/text-foundation-test.f` is
  `REG-COUNT 346 ASSERT=` — a ratchet on the number of `PRIM:`/`PPRIM:` rows
  in `src/core/checker.f`. No landing **in the window c68209e1..44aa8134**
  touches checker.f, and it is green standalone and in the
  lint-artifacts/fast pool phase at both ba7935d1 and 44aa8134. But see the
  section below: at 94bf026b it is a real red, and my "sibling refuted"
  conclusion was scoped to the wrong window.

## The real red that hid inside the incident

The combining lane bisected it: green at 44aa8134, RED at 94bf026b — the
no-emit merge, which added two `PPRIM: CHECKER-TAPE` axioms, `HOLD-ARM` and
`HOLD-DISARM` (`src/core/checker.f:5631-5632`), and did not bump the
ratchet. That lane saw exactly this one red phase, attributed it to the
contention incident, and it was merged on that attribution.

Verified here independently before fixing: the two rows are present at
5631-5632, the test fails at assertion 741 on current master, and a probe
using the test's own lexer path counts 348 registry rows in checker.f
(285 `PRIM:` + 63 `PPRIM:`, which agrees exactly). Ratchet bumped to 348
with a dated note naming the two words, following the precedent in commit
137fba7b. The old prose split — "283 plus 61" — never summed to the 346 it
sat beside; corrected to the measured 285 plus 63.

Both halves are real and they are independent. The contention class below
did not cause this red, and this red does not explain the contention
measurements.

## Process lesson

A phase that is red during a known incident must still have its ASSERTION
read before the red is charged to the incident. The assert number settles it
in one line: 741 is a content ratchet and can only move when source content
moves, so it is never a timing flake; 395-398 and 408 are ns/ms budgets and
are exactly what contention breaks. "The incident is open and this phase is
red" is not attribution — it is the absence of it. I made the mirror-image
error from the other side: I confirmed 741 green across c68209e1..44aa8134,
which was true, and reported the sibling refuted without noticing my window
stopped short of the commit that actually broke it.

## Root cause: perf ratchets judged by a probe blind to the contention

Reproduced by running two full suites at once on this host (master and
ba7935d1, separate workspaces, separate HB_TMP). BOTH went red, identically,
on two different trees that are green alone:

```
runtime elapsed-ms=18990 max-ms=17280 cal-pct=108   (master)
runtime elapsed-ms=18942 max-ms=17280 cal-pct=108   (ba7935d1)
TFAIL	assert	408	nested child-process group time
```

`test/gate-engine-lib.f:2179` decides `OVER?` against
`NOMINAL-MS TEST-BUDGET:PERF-MS` — 16000 ms scaled by `HB_CAL_PCT`. That
factor comes from `T-BUDGET-SELF-PCT` (`lib/test/budget.f:52`), a
single-threaded arithmetic spin (`T-BUDGET-CAL-SPIN`, line 36) with no fork,
no exec, no I/O. On a 10-core box a spin loop keeps a whole core while the
other nine are saturated, so the probe reported 108% — an 8% slowdown —
while the measured workload ran 73% slower. The ratcheted work is
fork/exec/reap bound (`process-fork=1808`, `process-exec=450`); the probe
measures the one resource that is NOT contended.

The clamp is not the problem: at cal-pct=108 the factor is nowhere near the
300% ceiling, so `SATURATED?` (line 2176) never fires and the slice fails
believing its budget was valid.

`lib/test/budget.f:4-8` already names this flake class ("concurrent gate
runs", dots habu-concurrent-multi-workspace-5341c7f4 and
habu-process-test-standalone-9de825bc). The tree already distinguishes
load-aware `HB_LOAD_PCT` (timeouts, `T-BUDGET-MS`) from spin-only
`HB_CAL_PCT` (perf ratchets, `TEST-BUDGET:PERF-MS`). The perf path
deliberately excludes load so a real regression is not masked — which is why
it must not render a FAIL verdict at all when it cannot separate "the code
got slower" from "the host is busy".

This is why every lane's own gates were green and merge-gate was red: lanes
run alone, merge-gate runs while lanes run. It is a design finding, not a
one-line repair — no budget number can be both tight enough to catch a
regression and loose enough to survive an arbitrary number of concurrent
lanes. The structural fix is a validity precondition on the measurement
(single live gate on the host), filed as habu-stop-perf-ratchets-217b617b.
Two further cross-lane leaks found on the way, both through the same
machine-global PERSIST$ root: habu-give-atomic-write-995c4408 and
habu-keep-the-red-5bbf5559.

## Caught live, in a second ratchet

While gating this very change the suite went red again, unprompted, on a
tree whose only diff is markdown under `.dots/`. At that moment:

```
load averages: 11.28 13.98 10.89
8 foreign `bin/hb --load test/run.f` processes from other lanes
```

The failing phase was `fork json-read-perf ratchets` — four ns-budget
assertions (395-398) whose fastest-of-five sample came in 9-11% over budget.
Its own diagnostic line is the whole finding in one string:

```
json-read-perf-phase: calibration pre=125 post=131 stable=true
saturated=false load1-pre-x100=n/a load1-post-x100=n/a
runnable-pre=n/a runnable-post=n/a admissible=true
```

Read that line carefully, because it falsifies its own source comment. The
load and runnable columns are `n/a` here — `LOAD-READ`
(`test/json-read-perf-phase.f:195`) reads a Linux-style load file that macOS
does not have — but they were never the decision anyway.
`test/json-read-perf-phase.f:122` says so outright: "These are AUDIT
CONTEXT, not the admissibility decision." The decision is `ADMISSIBLE?`
(line 219): the spin bracket did not drift more than ten percent, and the
factor is not clamped. Both held, so the phase admitted the measurement.

And the comment above `ADMISSIBLE?` claims exactly what the failure
disproves: "Both read the very resource the workloads compete for, at the
time they run, which is why neither is a load heuristic." They do not. The
spin (`T-BUDGET-CAL-SPIN`, `lib/test/budget.f:36`) is register-only
arithmetic over a working set of nothing. It reported the box 25-31% slow
and the budgets were scaled by that — yet the workloads still came in 9-11%
over, because JSON decode is memory- and cache-bound and eight concurrent
gates destroy exactly the bandwidth the spin cannot feel.

So the defect is one shape in two places, and it is not a threshold:

- runtime slice — spin proxy vs **fork/exec** throughput (cal-pct 108 while
  the work ran 73% slower)
- json-read-perf — spin proxy vs **memory/cache** bandwidth (scaled budgets
  still missed by 9-11%)

A single-resource proxy is being used to normalize workloads bound by other
resources. That is why no budget number and no wider clamp can fix it, and
why the honest precondition is about whether the host is exclusive at all,
not about how slow the probe felt.

## Not reproduced

The specific reported symptom — 'baseline validation shared', 3 failures —
did not reproduce in any configuration tried: not at any of the three
commits, not twice at master, and not under the 2x contention that did red
phases 16 and 13/35. The baseline worker run directly
(`HABU_UNDER_TEST=bin/hb bin/hb --load test/candidate-validation.f -- shared`)
exits 0 with all 46 cases matching. Under contention the same class of
failure is expected there too: `SUBJECT:RUN` gives each case 120 s
(`test/candidate-validation.f:26`) and a timed-out case makes `EXIT=?`
(line 134) false, which reports as "wrong negative outcome" on whichever
cases happened to be slowest — a count like 3 with no common cause between
them.
