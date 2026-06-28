---
title: Collapse stdlib tool subprocesses
status: open
priority: 1
issue-type: task
created-at: "2026-06-28T19:06:06.629889+02:00"
---

Problem: test/gate-stdlib.f TEST-SUITE and TEST-TOOL-SUITE still spawn bin/hb per suite, and the longest current tails are stdlib check-cli (55745ms) and tool-boundary (54790ms). Many tool tests already have core libraries and do not need a CLI process for every semantic case. Fix: split each long tool test into checked core semantic words plus a single CLI/boundary wrapper per tool family; run semantic suites inside the warm gate runner, keeping process tests only for argv/env/stdin/exit contracts. Files: test/gate-stdlib.f, tools/*-test.f for trust/check-all/public-signatures/stale-status/repl/diag-origin/aot/signature/checked-boundary/reserved/duplicate/typed-local. Acceptance: tool-boundary and check-cli slices lose avoidable inner hb launches; boundary coverage remains explicit; focused slice timing drops with full gate green.
