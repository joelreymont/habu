---
title: Apply scheduler result batch
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T07:39:44.639063+02:00"
blocks:
  - habu-preflight-scheduler-result-6aeba3bb
---

Why: a completely checked result set needs one total mutation pass distinct from validation and scheduler execution. Result: package-private SERVE:APPLY-BATCH consumes the unforgeable checked(server,results) proof whose private rows already contain every SERVE-CONN validated pair, projects each pair once in order, calls only total SERVE-CONN:APPLY, returns the updated server, and retains no result span. It performs no offset, slice, connection, request, or capacity validation and has no refusal arm. Owner: whole-result-batch application only. Production red: checked rows have no complete-set commit transition. Acceptance: zero, one, mixed token and terminal rows, several connections, and order-sensitive fragments apply all rows exactly once; deleting, reordering, or substituting a pair fails the production trace. Forbidden: validation, raw row projection, SCHED:TICK, socket I/O, fallible operation, allocation, retry, terminal scheduler transition, compatibility, metric, or lint. Smallest owning check: checked real SCHED result sets through SERVE-CONN:APPLY. Claim: unassigned.
