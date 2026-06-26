---
title: Factor bootstrap colon open
status: closed
priority: 1
issue-type: task
created-at: "\"2026-06-25T12:19:43.507788+02:00\""
closed-at: "2026-06-25T12:39:37.941460+02:00"
close-reason: "completed: split bootstrap/cg/forth.fs EMIT-INTERPRET-COLON into named phase helpers in commit 3e2e601f. Evidence: tools/bootstrap-codegen-test passed; filemap-lint passed; full native gate passed. No-binary recovery bootstrap was not run because local gforth 0.7.3 fails the documented {: :} locals probe and tools/bootstrap.sh exits 69 before touching bin/hb."
---

Finding F02. Evidence: docs/factorization-review.md:30; bootstrap/cg/forth.fs:2299. Root cause: EMIT-INTERPRET-COLON bundles colon token recognition, code/dict capacity exits, pending dict record setup, name storage, optional signature capture, compile-state reset, and prologue emission. Fix: split into focused words for colon token check, code room, dict room, pending record init, signature capture, state reset, and word prologue. Why: the open-definition path should be reviewable through small stack-commented phases. Validate with bootstrap-codegen-test, native fixpoint, and full native gate.
