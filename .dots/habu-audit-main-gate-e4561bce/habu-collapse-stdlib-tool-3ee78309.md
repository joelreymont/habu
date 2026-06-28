---
title: Collapse stdlib tool subprocesses
status: active
priority: 1
issue-type: task
created-at: "\"2026-06-28T19:06:06.629889+02:00\""
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
