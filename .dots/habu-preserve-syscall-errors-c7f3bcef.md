---
title: Preserve syscall errors through process I/O
status: open
priority: 1
issue-type: task
created-at: "2026-07-21T22:01:19.357191+02:00"
---

The native `SYS-PUSH` path reduces syscall failures to -1, so
`BREAD`/`BWRITE`/`BPOLL` callers cannot distinguish `EINTR`, `EAGAIN`, peer
closure, partial transfer, or permanent failure. Process fork can retry every
error indefinitely, process stdin can close on retryable errors, and PTY
release can publish launched without delivering exactly one byte.

Introduce an errno-preserving typed outcome at the read, write, and poll
primitive boundaries on both native targets and the recovery mirror. Provide
one shared exact-`EINTR` retry abstraction, explicit partial-transfer
progression, and distinct ready, timeout, closed, retryable, and permanent
states. PTY launch publishes success only after exactly one release byte is
written and observed; every failure keeps ownership and cleanup coherent.
Preserve raw errno and primary-versus-cleanup error identity. Iteration limits
and timing heuristics are forbidden.

Owner and files: engine syscall emitter/effects, the bootstrap mirror,
`lib/process*.f`, and focused PTY/process I/O tests. Acceptance injects `EINTR`
then success, `EAGAIN`, short reads/writes, zero/closure, persistent error,
signal interruption, release failure, and cleanup failure, plus native syscall
probes on both targets. Run fixpoint/bootstrap parity, process and PTY slices,
typed-local, package, and host gates, then the full native gate.
