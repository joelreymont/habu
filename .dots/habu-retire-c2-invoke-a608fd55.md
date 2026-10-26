---
title: Retire c2-invoke and the owner DRAIN
status: open
priority: 3
issue-type: task
created-at: "2026-10-27T22:17:04.269543+02:00"
---

Problem: c2-invoke and the owner AT-EXIT DRAIN existed because a task halted at TASK:PAUSE ended through pthread_exit without unwinding finally. A halt now unwinds through finally, and finally runs its cleanup under a catch, so c2-invoke is finally plus a finisher whose one job is serving a halt that stayed pending through the outermost scope's deferred close, carrying the scope's selected code (src/lib/c2-memory/owner-runtime.f FINISH calls TASK:HALT-UNWIND), and the DRAIN (owner-runtime.f DRAIN, REGISTER-EXIT; docs/design.md Ownership) has no remaining reachable case. Retiring c2-invoke touches src/habu/prims.f, src/core/checker.f, src/compiler/native/elaborate.f, hir-word.f, both emitters (habu1.f BC2-INVOKE, kernel-x64.f C2-INVOKE,) and test/c2-memory-runtime.f, test/c2-memory-refusals.f, diag-position-test.f. Acceptance: the owner surface (INIT-RUN, RECORDS-RUN, scoped bodies) runs on finally with no finisher and the outermost scope still serves a pending halt with its selected code (test/c2-owner-dispose.f "deferred halt preserves cleanup over body and exit errors without resuming catch"); c2-invoke and its checker, native and emitter rows are gone; the DRAIN is removed, or kept with a stated reachable case; docs/design.md states the result. Verification: test/c2-owner-dispose.f, test/c2-init-program.f, test/c2-field-loan-lifecycle.f, test/c2-records-lifecycle.f, test/c2-memory-runtime.f, test/c2-memory-refusals.f and src/lib/task-test.f pass on an engine built from the tip, then the full registry and the two-generation build.
