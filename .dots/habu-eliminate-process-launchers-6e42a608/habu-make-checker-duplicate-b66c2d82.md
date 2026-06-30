---
title: Make checker duplicate rejects catchable
status: closed
priority: 2
issue-type: task
created-at: "2026-06-30T08:19:54.062669+02:00"
closed-at: "2026-06-30T08:35:14.767214+02:00"
close-reason: "completed locally: duplicate checker rejection now throws rc 78, check-all-errors buffer emits duplicate diagnostic, package duplicate checker test runs in-process; self-refresh, focused tests, lints, and hot full local suite green; zed untouched"
---

Problem: dictionary/checker duplicate-definition negative tests still need a child check.f process because CHECK-ALL-ERRORS-BUF cannot catch the duplicate-definition path; focused proof: in-process duplicate package reject exits the whole process with fatal rc 78, while package no-return rejects are now catchable through VERIFY-SOURCE/CHECK-ALL-ERRORS-BUF. Root cause: checker duplicate definition uses a process-exit/die path instead of a checker verdict/diagnostic path under candidate/all-errors scope. Fix: change duplicate-definition checker rejection to be catchable under CHECK-CANDIDATE!/VERIFY-SOURCE/DIAG-BUFFER without weakening fail-closed CLI behavior; migrate GD-PACKAGE-DUPLICATE-CHECK to GD-CHECK-BUF-BAD; keep compile/runtime duplicate source execution boundary tests as child sentinels. Verify: focused duplicate buffer adapter returns rc 78 or mapped E-DUP diagnostics without process exit and stderr contains duplicate definition; direct dictionary/checker phase green; full local native suite green hot under 30s. Zed/device validation out of scope.

2026-06-30 proof: local native self-refresh passed with compiler fixpoint and small checked engine ready. `tools/check-all-errors-test` passed, `tools/check-test` passed with the duplicate CLI boundary regression, direct native dictionary/checker phase passed, cold full local suite passed 43327ms internal / 45.501s wall, and final hot full local suite passed 24741ms internal / 26.921s wall with `inner-hb-stdin=19`, `boundary=50`, `helper-spawn=90`, and `warm-miss=0`. Zed/device validation intentionally untouched because another agent owns zed/Odin work.
