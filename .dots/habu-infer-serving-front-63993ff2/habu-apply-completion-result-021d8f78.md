---
title: Apply completion result
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T01:12:14.473323+02:00"
blocks:
  - habu-infer-serve-http-4fb09e9a
  - habu-render-completion-json-9fff2d34
  - habu-infer-scheduler-bounded-53574658
---

Why: scheduler rows must update exactly one matching connection without mixing transport I/O into scheduler ownership. Interface: SERVE-CONN:CHECK-APPLY validates one connection, typed SCHED row, checked byte slice, request state, and the response capacities proven before SUBMIT, returning both values unchanged on success or refusal. SERVE-CONN:APPLY accepts only that validated pair; token appends within the proven output bound, token-final and finished append then render once, and failed renders one bounded error. Every copy and render is total after CHECK-APPLY, so a caller can preflight a complete result batch before changing any connection. A terminal apply leaves one stable JSON body for WRITABLE and never retains result storage. Owner: scheduler-result validation and connection-state transition only. Production red: no product path consumes a result row into a response. Acceptance: token, token-final, finished, failed, stale, duplicate, wrong-connection, exact-capacity, and one-short cases reject or apply exactly; one-short refusal occurs before SUBMIT or CHECK-APPLY and mutates nothing; after a successful check no apply or renderer failure arm exists. Forbidden: scheduler tick, socket read or write, cancellation, allocation, result copy, retry, version, or compatibility mapping. Smallest owning check: bin/hb --load maki/serve/connection-result-test.f with real SCHED rows.
