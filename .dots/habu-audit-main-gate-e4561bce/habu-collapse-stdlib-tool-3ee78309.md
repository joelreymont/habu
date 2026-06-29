---
title: Collapse stdlib tool subprocesses
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-06-28T19:06:06.629889+02:00\\\"\""
closed-at: "2026-06-29T02:55:07.865869+02:00"
close-reason: implemented checked core/entry splits for repl-lint and bundle-lib; removed duplicate repl-lint CLI smoke from tool-boundary and made bundle success in-process; focused hot tool slice 22.81s wall / lints 16.717s; full hot gate passed 44.811s internal / 47.81s wall
---

Problem: test/gate-stdlib.f TEST-SUITE and TEST-TOOL-SUITE still spawn bin/hb per suite, and the longest current tails are stdlib check-cli (55745ms) and tool-boundary (54790ms). Many tool tests already have core libraries and do not need a CLI process for every semantic case. Fix: split each long tool test into checked core semantic words plus a single CLI/boundary wrapper per tool family; run semantic suites inside the warm gate runner, keeping process tests only for argv/env/stdin/exit contracts. Files: test/gate-stdlib.f, tools/*-test.f for trust/check-all/public-signatures/stale-status/repl/diag-origin/aot/signature/checked-boundary/reserved/duplicate/typed-local. Acceptance: tool-boundary and check-cli slices lose avoidable inner hb launches; boundary coverage remains explicit; focused slice timing drops with full gate green.

Checkpoint 2026-06-28: `tools/check-test.f` now runs the pure positive
verification fixture through `VERIFY-SOURCE-BUF` and the unterminated-string
diagnostic through `CHECK-ALL-ERRORS-FILE` in-process. The remaining spawned
`tools/check.f` cases are explicit CLI/boundary contracts: file-label JSON,
usage, runtime `die` propagation, source-list reserved-name labeling, and
source-list audited-lib success. Focused `check-cli-boundary` fell from 30.148s
in the prior full gate tail / 29.92s focused wall to 16.222s internal /
26.04s focused wall with the same warm cache. The full hot gate passed at
47.260s internal / 50.05s wall with `warm-hit=16`, `maker-hit=1`,
`candidate-hit=1`, and `helper-spawn=113`; the remaining critical path is
engine fixtures at 35.977s, stdlib tool-boundary at 29.505s, and AOT-positive
at 26.788s under contention.

Checkpoint 2026-06-29: `tools/repl-lint.f` and `tools/bundle-lib.f` now have
checked core/entry splits. `repl-lint-test` runs good/bad scanner semantics
in-process and relies on the existing standalone `repl-lint` suite for the CLI
wrapper. `bundle-lib-test` builds the successful bundle in-process while
keeping missing-input and generated-bundle execution as process boundaries.
Focused hot `test/gate-stdlib.f -- tool` fell from the earlier 25.79s wall /
19.917s lints tail to 22.75s wall / 16.851s lints tail before full-DAG
validation. A follow-up split of `tool-boundary-lints` into three parallel
suites improved focused wall to 18.29s, but hot full gate regressed slightly
and added two top-level subprocesses, so the split was reverted.
