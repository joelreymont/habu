---
title: Apply completion result
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T01:12:14.473323+02:00"
blocks:
  - habu-open-completion-conn-497b7b44
  - habu-render-completion-json-9fff2d34
  - habu-infer-scheduler-bounded-53574658
---

Why: scheduler rows must update exactly one matching connection without mixing transport I/O into scheduler ownership. Interface: SERVE-CONN:CHECK-APPLY accepts only a batch-authorized row and byte view minted by SERVE:CHECK-BATCH; it validates the matching connection and request state plus every response capacity proven before SUBMIT, and never rechecks table count, offsets, overlap, or byte-slice bounds. SERVE-CONN:APPLY accepts only the resulting validated pair; token rows append within the proven output bound. Token-final and finished rows clear the connection writer, call OPENAI-COMP:RENDER, and copy one complete stable body. Failed rows clear it, call RENDER-ERROR, and copy one complete stable error. Writer clear, render, and copy are total after the complete batch check; the writer remains connection-owned until close and no result span is retained. Owner: connection/request validation and connection-state transition only. Production red: no product path consumes one batch-authorized result row into its matching response. Acceptance: token, token-final, finished, failed, stale, wrong-connection, exact-capacity, and one-short cases reject or apply exactly; raw or independently formed slices cannot call CHECK-APPLY; a complete batch refusal changes no connection; after successful checks no append, render, copy, or writer failure arm exists; two connections retain independent writers. Forbidden: batch count, offset, overlap or slice-bounds validation, scheduler tick, socket read or write, cancellation, allocation, result copy, retry, version, compatibility, metric, or lint. Smallest owning check: bin/hb --load maki/serve/connection-result-test.f with real SCHED rows.
