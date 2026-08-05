---
title: Make host tuning a restorable transaction
status: open
priority: 2
issue-type: task
created-at: "2026-07-21T22:04:23.246642+02:00"
---

Host profile instructions can change Jetson clocks or power state while suppressing save failures and provide no mandatory restore path. Define a package-owned host-tuning transaction that probes the exact device, captures every state it will change, validates the snapshot, applies a requested profile, verifies the resulting state, and guarantees restore on success, failure, signal, or timeout. If any state cannot be captured or restored, no tuning or performance evidence is permitted. Bind the before/applied/restored facts and device identity into the measurement evidence. Add injected capture, partial apply, verification, benchmark, cleanup, restore, and simultaneous primary-plus-restore failures; prove reverse restoration and explicit degraded-host diagnostics. Repository docs and skills call this API rather than open-coded commands. Files: checked host profile/tuning owner, tests, generated skill/doc adapter. Verify off-device fake backend, live configured-host smoke where available, performance evidence gate, host/dot lints.
