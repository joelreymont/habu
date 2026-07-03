---
title: Batch candidate-source probes
status: open
priority: 2
issue-type: task
created-at: "2026-07-01T22:33:20.214717+02:00"
---

Hot gate run still spawns 25 helper processes (measured from gate-stats.tsv: 18x bin/hb + 5x candidate) at 0.3s base + 1.2-1.8s prelude each; boundary tests dominate group tails (lib/process-test.f 3.7s, tools/check-all-errors-test.f 4.5s, tools/ptx/saxpy-test.f 3.7s). gate.md items 7-8 are the acknowledged unbuilt fix. Fix: one candidate launch runs a batched probe list (probe table in, rc/stdout/stderr per probe out) instead of one spawn per contract; in-process source-misuse negatives once the evaluator returns results (depends habu-evaluator-result-obj-2cf9b484).

## Status (2026-07-02, opus-tools)

The high-value half is dependency-blocked and the remaining half is now
low-yield:
- In-process source-misuse negatives (the batched probe table with rc/stdout/
  stderr per probe) require the evaluator to return a catchable result union
  instead of `die`/native-exit. That is dot habu-evaluator-result-obj-2cf9b484,
  which is still `status: open`. Until it lands, a source program that hits a
  runtime-compiler error terminates the process, so the negatives MUST stay as
  process sentinels (docs/gate.md Boundary Rule) — they cannot be batched into
  one resident launch.
- The candidate-source subject is already mostly in-process on this tree:
  dictionary negatives use the transactional resident `CHECK-CANDIDATE!` (no
  spawn), and a hot run reports only `inner-hb=1 inner-hb-stdin=4` with the
  candidate-source subject at spans=2 total-ms≈1313. The residual candidate
  spawns are `GE-CANDIDATE-VALIDATE` (test/gate-engine-lib.f): one
  `GE-ENGINE-SUITE-ON` launch of the candidate (engine-suite.f, already a batched
  bundle of T{}T probes) + one `GE-CAND-SMOKE` launch. Folding GE-CAND-SMOKE's 4
  probe lines into the candidate engine-suite stdin feed would save exactly one
  candidate launch, but it entangles two separate output contracts (engine-suite
  asserts trailing "ok"; smoke asserts specific line outputs) in one stdin/stdout
  stream — real correctness risk for a ~1-spawn gain, and the durable batching
  design (probe table -> per-probe rc/out/err) only pays off once the evaluator
  result API exists. Recommend implementing after
  habu-evaluator-result-obj-2cf9b484 so the batched probe table is built once,
  in-process, for both the negatives and the smoke, rather than a fragile
  merged-stdin micro-optimization now.

## Done (2026-07-03, opus-engine)

With the evaluator result object landed (habu-evaluator-result-obj-2cf9b484),
folded the residual candidate smoke into the engine-suite candidate launch so the
candidate-source probes share ONE `HABU_UNDER_TEST` spawn (gate.md item 8):

- `GE-CAND-SMOKE` / `GE-CAND-SMOKE-SOURCE` / `GE-CANDIDATE-HOOK-CHECKS` removed
  from `test/gate-engine-lib.f` (they were a second candidate launch).
- The three smoke checks (boot check-hook installed, checked def compiles+runs,
  representative baked primitive resolves) are now independent `T=` probes inside
  `test/engine-suite.f` after the hook is restored, so they ride the existing
  `GE-ENGINE-SUITE-ON` candidate launch and also run on `bin/hb`. `T=` per probe
  is the durable "probe table -> per-probe pass/fail" the dot wanted, with NO
  entangled stdout contract (the risk the earlier note flagged for a stdin merge);
  the result-object makes a probe error non-fatal to the shared candidate batch.

Measured hot gate (macos-arm64-10x2), before -> after: helper-spawn 26 -> 23,
candidate 1 -> 0, inner-hb-stdin 4 -> 3, boundary 5 -> 4; full gate green,
engine-suite passes on both the candidate and bin/hb, byte-for-byte fixpoint
unchanged (test-only change).

Remaining (kept open): a general candidate-side batched probe RUNNER (probe table
in, rc/stdout/stderr per probe out) for source-misuse NEGATIVES. Blocked in this
tree only for negatives whose runtime-compiler error is a raw `NR-EXIT-GROUP`
(duplicate-definition `$4E`, colon/dict overflow, package misuse) rather than a
`throw` — habu-evaluator-result-obj converted the `throw`-based path (checker HOOK
reject), and pure checker negatives already run in-process via `CHECK-CANDIDATE!`.
Converting the raw-exit sites to catchable throws is engine work in the FIND/HIDX
+ colon regions and should be its own dot before the general negative-probe batch.
