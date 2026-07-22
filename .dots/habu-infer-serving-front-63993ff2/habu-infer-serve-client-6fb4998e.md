---
title: "Infer serve: client backpressure lease"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T10:07:43.649057+02:00"
blocks:
  - habu-infer-serve-engine-37b8873a
  - habu-infer-scheduler-cancellation-523c6cb8
---

Why this exists:
A slow or disconnected client must not grow unbounded output or retain engine and key/value-cache state indefinitely.

Required result:
Give each client a bounded output lease with explicit writable, blocked, cancelled, and closed transitions connected to scheduler cancellation.

Done when:
Blocked clients stop receiving new buffered events at the declared bound; disconnect and timeout cancel exactly once; resumed writes preserve order; every terminal path releases request, output, and cache ownership.

Expected touch points: client output lease, scheduler connection, and focused state tests.
Smallest check: the focused slow-client and disconnect trace.
Prerequisites: engine event stream and scheduler cancellation.
Owned result: per-client output backpressure and cancellation only.
Claim: unassigned.
