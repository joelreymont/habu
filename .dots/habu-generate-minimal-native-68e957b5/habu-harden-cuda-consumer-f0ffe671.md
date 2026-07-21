---
title: Harden CUDA consumer lifecycle
status: closed
priority: 2
issue-type: task
created-at: "2026-07-19T20:46:08.234573+02:00"
closed-at: "2026-07-21T21:37:08+02:00"
close-reason: "Superseded: the unbuilt Zig kernel consumer was deleted, so this consumer-only lifecycle work has no remaining code owner. No replacement host-language example, compatibility shim, or follow-up task was created."
---

This task described lifecycle, cleanup, digest, and output-validation work for
the former unbuilt Zig demonstration. That consumer has been removed, so the
task has no remaining implementation surface and is superseded without a
replacement consumer.

Historical finding retained as evidence: `examples/kernel-consumer/main.zig`
discarded CUDA cleanup failures, used unproved fixed-buffer formatting, and
validated only one output element. The task would have required owned resource
wrappers, explicit error precedence, exact formatting, full output validation,
and injected-driver tests if that example had remained an owned repository
surface.
