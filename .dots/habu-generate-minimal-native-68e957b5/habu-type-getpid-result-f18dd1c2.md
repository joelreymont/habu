---
title: Type getpid result
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T22:24:08.759274+02:00"
---

Current master type-model defect in new process identity primitive: src/core/checker.f:4990 comments getpid as ( -- pid ) but registers PE-N, so checked code and test/getpid-smoke.f:30-38 see an interchangeable number. lib/process-pty-handle.f:232,240 and tests repeatedly repair the primitive result through >PID or compare PID>N to raw getpid, spending trusted casts and permitting PID/count/descriptor misuse between the syscall and wrapper. Unlike fork/spawn/wait raw syscalls, getpid cannot return a negative error and already has one semantic role. Add a primitive-effect constructor for the existing pid role and model getpid directly as ( -- pid ) across checker, source/recovery mirrors, primitive-effect recipes, AOT/snapshot/fixpoint metadata, and diagnostics; if the primitive table cannot yet carry declared nominals, add one checked package-owned SELF word that performs the sole audited refinement and make raw getpid a private/trusted boundary unavailable to ordinary checked callers. Migrate PTY and every non-boundary consumer to the typed result with no >PID/PID>N round trip. Add checked positives for equality/same-process/fork difference and negatives for pid vs n/fd/rc/other nominal roles; prove primitive emitted bytes/syscall behavior unchanged, raw error-returning process primitives remain n where required, trust inventory shrinks or does not grow, and getpid/PTy/process/bootstrap/fixpoint/full native gates pass. Measure wrapper/consumer JIT/CODELEN before/after. Coordinate declared primitive-role capability with current checker-effect owners; this dot owns only getpid semantic output.
