---
title: Submit completion request
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T07:03:54.402757+02:00"
blocks:
  - habu-preflight-completion-req-fe9058a8
  - habu-infer-scheduler-fifo-88f80a53
---

Why: a capacity-proven connection must authenticate its scheduler before installing a request handle. Result: SERVE-CONN:SUBMIT consumes the healthy scheduler and one preflighted connection/request, calls SCHED:MATCH-ID with the connection-derived id, then calls the sole SCHED:SUBMIT with prompt bytes, maximum output, SAMPLE:config, and seed. Mismatch or submit refusal returns both owners before installing a handle; success stores only the returned matching nominal request handle and enters waiting state. Owner: scheduler authentication and request submission only. Dependencies: completion preflight and FIFO SUBMIT owner. Production red: a connection can otherwise install a handle from a different scheduler. Acceptance: two schedulers and two bound connections accept only matching pairs; full rows, KV full, malformed prompt, invalid sampling, stale identity, and every submit refusal leave connection and scheduler exact; success installs one handle once. Forbidden: read, decode, capacity calculation, response write, result apply, second request, retry, compatibility, metric, or lint. Smallest owning check: real SCHED SUBMIT through maki/serve/connection-submit-test.f.

Claim: unassigned.
