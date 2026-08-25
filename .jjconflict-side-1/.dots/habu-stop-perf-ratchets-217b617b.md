---
title: Stop perf ratchets judging a contended host
status: open
priority: 2
issue-type: task
created-at: "2026-08-06T17:55:10.765275+02:00"
---

Reproduced 2026-08-06: two full test/run.f runs at once on one Mac (master 44aa8134 and ba7935d1, separate workspaces and HB_TMP) BOTH go red on trees that are green alone. test/gate-engine-lib.f:2179 OVER? compares elapsed against 16000ms scaled by HB_CAL_PCT; that factor is T-BUDGET-SELF-PCT (lib/test/budget.f:52), a single-threaded arithmetic spin with no fork/exec/IO. Measured: runtime elapsed-ms=18990 max-ms=17280 cal-pct=108 -- the probe saw an 8 percent slowdown while the workload ran 73 percent slower, because a spin loop keeps a whole core while the other nine are saturated and the ratcheted work is fork/exec/reap bound (process-fork=1808, process-exec=450). SATURATED? (line 2176) never fires at 108 percent, so the slice fails believing its budget was valid. Same event reds the tail-process group (assert 408 'nested child-process group time'). No budget number fixes this: it must be tight enough to catch a regression and loose enough for N concurrent lanes. Needs a validity precondition on the measurement -- a structural check that this is the only live gate on the host (PERSIST$ is already a machine-global root that could carry the marker) -- with the ratchet reporting and abstaining otherwise, never inflating the budget. lib/test/budget.f:4-8 already names this flake class. This is why lanes are green alone and merge-gate is red.

Start here, because the mechanism already exists and only fails open. The
json-read-perf phase caught the same contention live during this dot's own
gate run and printed its admissibility decision:

  json-read-perf-phase: calibration pre=125 post=131 stable=true
  saturated=false load1-pre-x100=n/a load1-post-x100=n/a
  runnable-pre=n/a runnable-post=n/a admissible=true

The load1/runnable columns are n/a because LOAD-READ
(test/json-read-perf-phase.f:195) reads a Linux-style load file macOS lacks,
but they were never the decision: line 122 says "These are AUDIT CONTEXT,
not the admissibility decision". ADMISSIBLE? (line 219) is spin-bracket
drift under ten percent AND factor not clamped. Both held, so it admitted --
while the box ran load 11.28 under eight foreign 'bin/hb --load test/run.f'
processes and assertions 395-398 missed their ns budgets by 9-11 percent.

The comment above ADMISSIBLE? claims "Both read the very resource the
workloads compete for, at the time they run, which is why neither is a load
heuristic." The failure disproves it. T-BUDGET-CAL-SPIN is register-only
arithmetic over a working set of nothing; it reported 25-31 percent slow,
the budgets were scaled by that, and the workloads STILL missed by 9-11
percent because JSON decode is memory- and cache-bound and concurrent gates
take exactly the bandwidth the spin cannot feel. Same shape as the runtime
slice, different missed resource: there the proxy misses fork/exec
throughput (cal-pct 108 while the work ran 73 percent slower), here it
misses memory bandwidth.

So the fix is not a wider clamp, a load threshold, or a macOS load probe --
a proxy for one resource cannot normalize workloads bound by others. The
precondition has to be exclusivity: a perf ratchet may render a verdict only
when this gate is the only one on the host, and must report-and-abstain
otherwise. That is an existence check (a machine-global live-gate marker;
PERSIST\$ is already such a root) rather than any measured value. Route the
runtime slice (test/gate-engine-lib.f:2179), the tail-process group ratchet,
and json-read-perf's ADMISSIBLE? through that one gate, and delete the claim
in the comment that the spin reads the contended resource.
