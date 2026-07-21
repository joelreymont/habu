---
title: Preserve syscall errors through process I/O
status: open
priority: 1
issue-type: task
created-at: "2026-07-21T22:01:19.357191+02:00"
---

The native SYS-PUSH path reduces syscall failures to -1, so BREAD/BWRITE/BPOLL callers cannot distinguish EINTR, EAGAIN, peer closure, partial transfer, or permanent failure. process-fork can retry every error indefinitely, process stdin can close on retryable errors, and PTY release can publish launched without delivering exactly one byte. Introduce an errno-preserving typed outcome at the primitive boundary for read, write, and poll on both native targets and the recovery mirror. Provide one shared exact-EINTR retry abstraction, explicit partial-transfer progression, and distinct ready/timeout/closed/retryable/permanent states. PTY launch publishes success only after exactly one release byte is written and observed; every failure keeps ownership and cleanup coherent. Preserve raw errno and primary-versus-cleanup error identity. Add injected sequences for EINTR then success, EAGAIN, short reads/writes, zero/closure, persistent error, signal interruption, release failure, and cleanup failure, plus native syscall probes on both targets. Iteration limits and timing heuristics are forbidden. Files: engine syscall emitter/effects and bootstrap mirror, lib/process*.f and PTY I/O focused tests. Verify fixpoint/bootstrap parity, process/PTY/gate slices, typed-local/trust/package/host/filemap/dot lints, and full native gate.
