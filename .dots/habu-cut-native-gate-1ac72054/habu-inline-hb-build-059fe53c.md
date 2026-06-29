---
title: Inline hb-build lints
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-06-29T03:09:10.575768+02:00\\\"\""
closed-at: "2026-06-29T03:25:07.347837+02:00"
close-reason: "completed: added typed hb-build lint hooks plus narrow direct-lint adapter for AOT-positive; rejected warm-runner lint-core bake after user-sig snapshot overflow; focused hb-build and AOT-positive pass; cold full gate 71.927s, hot full gate 43.261s internal / 46.29s wall with cache hits; boundary reduction is neutral on total hot wall"
---

Problem: tools/hb-build-lib.f HBB-BUILD still spawns child Habu for strict signature lint and AOT lint even when a gate caller has already loaded tools/signature-lint-core.f and tools/aot-lint-core.f. This makes AOT-positive pay duplicate process/compile cost. Fix: keep hb-build-lib defaulting to child CLI lints, add typed defer hooks, and load a narrow tools/hb-build-direct-lints.f adapter only on the cold AOT-positive load path; do not bake the lint cores into hb-gate-warm because that overflows the checker user-signature snapshot. Files: tools/hb-build-lib.f, tools/hb-build-direct-lints.f, test/run.f, FILEMAP.md, tools/filemap-lint.f. Verify: focused AOT positive, hb-build fixtures, typed-local-diff-lint, host/filemap/stale/dot lints, full hot native gate before master.

Checkpoint 2026-06-29: rejected the first direct-dependency shape after full
gate failed building `hb-gate-warm` with `checker: user sigs snapshot too large`.
Accepted hook adapter shape: default public hb-build path still uses child CLI
lints; AOT-positive loads `tools/hb-build-direct-lints.f` after lint cores and
installs typed execution-vector hooks. Focused default hb-build fixture passed.
Focused AOT-positive passed at 19.97s. Cold-after-change full gate passed at
71.927s (expected runner/maker/candidate cache rebuild). Hot full gate passed at
43.261s internal / 46.29s wall with `warm-hit=16`, `maker-hit=1`,
`candidate-hit=1`, and unchanged top-level process counts. This is a boundary
reduction, not yet a total hot-gate win.
