---
title: Factor regex token dispatch
status: closed
priority: 3
issue-type: task
created-at: "\"2026-06-25T12:19:43.579958+02:00\""
closed-at: "2026-06-25T22:00:41.299239+02:00"
close-reason: Factored regex metacharacter classification into checked byte-table helpers and split regex close/consume state transitions into named helpers. Added token-table and single-token emitter fixture coverage. Validated regex-test, stdlib-manifest-test, public-signatures-test, test/gate-stdlib.f, direct stale-status lint, and full native gate.
---

Finding F22. Evidence: docs/factorization-review.md:50; lib/regex.f:39, lib/regex.f:137, lib/regex.f:399, lib/regex.f:442. Root cause: regex classification and state transitions are long ad hoc dup/over token chains. Fix: factor token predicates or a small table, then split scanner dispatch and state update helpers. Why: regex behavior needs small tested pieces rather than stack juggling. Validate with regex tests and full native gate.
