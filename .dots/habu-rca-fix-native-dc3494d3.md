---
title: RCA+fix native gate budget overrun on 4-core Orin
status: open
priority: 2
issue-type: task
created-at: "2026-06-28T00:35:19.157141+02:00"
---

Static/measured RCA: the full native port gate (test/run.f) FAILS its own wall-clock budget on this 4-core Orin NX, INDEPENDENT of any code change. Pre-existing on master 2d535bd7 (the gate loads only lib/* + test/gate-pool.f + test/run.f; loads none of the redadd tools/docs that surfaced it).

Measured (this Orin, nproc=4):
- budget default = $15F90 = 90000ms (test/run.f:8 TR-DEFAULT-BUDGET-MS); env override HABU_GATE_BUDGET_MS exists.
- HABU_GATE_POOL_SLOTS default = 8 (test/gate-pool.f:6), GT-POOL-MAX=12.
- All 18 slices PASS individually; only the aggregate wall-clock fails.
- slots=4: 108349ms (under-parallel: each slice faster, less overlap)
- slots=8: 96287-101942ms
- slots=12: 96529ms (saturated; no gain past 8)
- Floor ~96s; long pole = native stdlib check-cli slice (~68s) + ~28s non-overlapping critical-path work.

Conclusion: calibration mismatch, NOT a code regression - the 90s default + 8-slot default were 'tuned by measurement' (commit a8365a3e) on a faster/more-core reference host; this 4-core Orin's floor is ~96s.

Correct fixes to evaluate (hard path preferred):
1. Cut the critical path so it fits <90s with margin on the 4-core Orin: start the check-cli long pole FIRST so all others overlap it; or split check-cli into smaller slices; profile what the ~28s non-overlapping tail is. Touches test/gate-pool.f scheduling.
2. If the gate host is officially the 4-core Orin, recalibrate TR-DEFAULT-BUDGET-MS to the measured floor + margin and document it; do NOT just bump to hide a real regression.

Files: test/run.f:8,62-92; test/gate-pool.f:5-6,172,206. Verify: native port gate passes <= budget with margin on this Orin at the documented slot count, all 18 slices still green. Do not bypass/shrink slices.
Note: NOT a blocker for tools/ptx + docs-only changes, whose owning gate is maki + lints + touched ptx slice (AGENTS.md), not the full native port gate.
