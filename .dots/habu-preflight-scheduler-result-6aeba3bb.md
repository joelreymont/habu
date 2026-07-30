---
title: Preflight scheduler result batch
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T07:39:44.479044+02:00"
blocks:
  - habu-own-completion-srv-dfc8812e
  - habu-apply-completion-result-021d8f78
---

Why: no connection may change until every scheduler row and arena slice in one tick is valid. Result: package-private SERVE:CHECK-BATCH consumes and returns one server plus the complete SCHED result set, verifies table count, every offset and byte-slice bound, nonoverlap, and unique request and connection targets, and privately mints the only batch-authorized row views. It passes those views to SERVE-CONN:CHECK-APPLY for connection/request state and response-capacity validation, then returns one unforgeable checked(server,results) proof or refused(server,results,error) without mutation. APPLY-BATCH alone projects the authorized views from that proof; no raw or independently constructed row view reaches connection apply. Owner: whole-result structure, slice authorization, and complete pre-mutation validation only. Production red: a later bad row can otherwise follow an already-applied prefix or a second owner can reinterpret arena offsets. Acceptance: zero, one, mixed, duplicate, stale, unknown, wrong connection, overlapping or short arena slices, wrong count, forged row view, connection-state failure, and last-row failure leave all connections byte-identical. Forbidden: SCHED:TICK, duplicate connection-state or capacity rule, row apply, connection mutation, copy, allocation, retry, terminal transition, compatibility, metric, or lint. Smallest owning check: real SCHED result sets through SERVE-CONN:CHECK-APPLY. Claim: unassigned.
