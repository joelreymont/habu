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

Why: scheduler rows must update exactly one matching connection without mixing transport I/O into scheduler ownership. Interface: SERVE-CONN:APPLY takes one connection, one typed SCHED result row, and its checked byte slice. token appends only within the preflighted output bound; token-final and finished append then render once; failed renders one bounded error; stale or mismatched handles reject. A terminal apply leaves one stable JSON body for WRITABLE and never retains result storage. Owner: scheduler-result to connection-state transition only. Production red: no product path consumes a result row into a response. Acceptance: token, token-final, finished, failed, stale, duplicate, wrong-connection, exact-capacity, and one-short fixtures preserve rows and connections exactly; renderer failure publishes no partial body. Forbidden: scheduler tick, socket read or write, cancellation, allocation, result copy, retry, version, or compatibility mapping. Smallest owning check: bin/hb --load maki/serve/connection-result-test.f with real SCHED rows.
