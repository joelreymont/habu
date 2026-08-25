---
title: Close completion connection
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T07:03:53.963643+02:00"
blocks:
  - habu-open-completion-conn-497b7b44
  - habu-infer-scheduler-cancellation-523c6cb8
---

Why: ordinary connection teardown must cancel one live request and consume the writer and descriptor exactly once. Result: SERVE-CONN:CLOSE operates only between scheduler ticks. For a live handle it calls SCHED:CANCEL once, closes the JSON-WRITE:writer once, then attempts SOCK-OS:CLOSE once even when cancellation refuses. Its arms return closed(scheduler,buffers), close-failed(scheduler,buffers,sock-error), or closed-with-errors(terminal,buffers,live-handle,error-set); every arm consumes the connection, writer, and descriptor, and a failed descriptor close is never retried. Idle close uses the same transition without cancellation. Owner: healthy-scheduler connection close only. Dependency: open completion connection and scheduler cancellation. Production red: a live request, writer, or descriptor can otherwise outlive its connection. Acceptance: idle and every live state, cancellation refusal, writer close, descriptor failure, partial-open cleanup, double close, and two peer connections release exact owners once; a peer is unchanged. Forbidden: terminal-scheduler close, read, write, decode, result apply, socket retry, compatibility, metric, or lint. Smallest owning check: real SCHED cancellation and SOCK-OS close through maki/serve/connection-state-test.f.

Claim: unassigned.
