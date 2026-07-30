---
title: Finish completion server stop
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T07:31:05.098691+02:00"
blocks:
  - habu-close-completion-srv-725dc7e0
  - habu-infer-scheduler-req-1ac1dac6
---

Why: after all connection rows drain, listener and scheduler ownership need one final ordered shutdown. Result: SERVE:STOP composes CLOSE-ROWS, consumes the listener with one SOCK-OS:CLOSE attempt, and calls SCHED:STOP. Success returns the INFER engine plus every caller storage owner and accumulated close errors. Scheduler refusal returns a STOP-only owner containing the exact SCHED:terminal and storage; retry calls only SCHED:STOP because all descriptors and writers are already consumed. Owner: listener and scheduler final shutdown plus STOP-only retry only. Dependency: closed server rows and scheduler terminal reclamation. Production red: a stopped server cannot return its engine and storage or safely retry cleanup. Acceptance: healthy, terminal, listener-close failure, scheduler refusal, retry success, double stop, immediate port reuse, and exact owner census pass through SERVE:STOP. Forbidden: connection walk logic, socket retry, engine stop, model close, polling, allocation, compatibility, metric, or lint. Smallest owning check: the final STOP slice of maki/serve/server-test.f. Claim: unassigned.
