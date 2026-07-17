---
title: Batch runtime source probes
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-17T15:28:56.928943+02:00\""
---

Parent habu-restore-30-second-fcadd9b9. Exact focused runtime proof: /private/tmp/habu-final-fields-34c4deb6/runtime.tsv, 21.56s and 103 counted helpers. test/gate-engine-lib.f:1832-1857 launches 102 candidate processes plus one /bin/sleep: uncaught4, interp-layout9, construct4, match6, dict-full1, div/mod2, pty1, argv3, underflow1, deref4, nested2, eval-undef3, eval-interp4, eval-def5, orphan12, CF5, rawexit10, residual20, package4, set-check2. Implement a single exact-candidate resident source runner with fork isolation for related cases; preserve real cold/top-level process sentinels where EVALD=0, argv, PTY, timeout, signal, or stdin/loader semantics are the contract. Preserve per-case stdout/stderr/outcome assertions and source identity. Acceptance: runtime helper-spawn <=12, focused runtime <=10s, every existing negative/positive passes, no boundary-class case converted to evaluate semantics, and full helper-spawn <=25 with the validation batch.
