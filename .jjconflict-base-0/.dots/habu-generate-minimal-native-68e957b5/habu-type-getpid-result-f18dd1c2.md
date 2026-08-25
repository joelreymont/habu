---
title: Type getpid result
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T22:24:08.759274+02:00"
---

Current type-model defect: src/habu/habu1.f declares and registers BGETPID, but src/core/checker.f models getpid with PE-N, so checked code and test/getpid-smoke.f see an interchangeable number. lib/process-pty-handle.f and its tests repair the primitive result through >PID or compare PID>N to raw getpid, permitting PID/count/descriptor misuse between the syscall and wrapper. Unlike fork, spawn, and wait primitives, getpid cannot return a negative error and has one semantic role. Add the nominal pid effect in the current checker primitive declarations and recovery mirrors, then migrate PTY and every ordinary consumer to the typed result with no >PID/PID>N round trip. Add checked positives for equality, same-process identity, and fork difference plus negatives for pid versus n, fd, rc, and other nominal roles. Prove the emitted bytes and syscall behavior unchanged, raw error-returning process primitives remain n where required, and test/getpid-smoke.f, lib/process-pty-handle-test.f, process, bootstrap, fixpoint, and full native gates pass. Measure wrapper and consumer JIT/CODELEN before and after. Coordinate the declared primitive-role capability with the checker-effect owner; this dot owns only getpid semantic output.
