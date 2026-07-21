---
title: Lease KV snapshot publication
status: open
priority: 1
issue-type: task
created-at: "2026-07-21T23:32:48.534830+02:00"
blocks:
  - habu-add-immutable-lexical-28b79e06
---

Problem: the rejected KV remainder copies block tables into two ordinary host buffers and flips an index without a buffer lease, page pin, device acknowledgement, or proved release/acquire operation. Generation N+2 can overwrite the buffer still read by generation N, and cancellation can recycle a page while an in-flight snapshot still names it. Fix: after the common MEM owner/subspan/immutable-borrow chain lands, define a generation-bearing linear snapshot lease over explicitly GPU-visible storage. Publishing fills an unleased buffer, pins every referenced page, performs the host-to-device release operation, and returns the only launch authority. Completion consumes a CUDA event or equivalent stream acknowledgement, unpins pages, and returns the buffer lease before reuse. Generation and page identities exhaust before reuse. Acceptance: republish over an in-flight buffer, free/reuse of pinned pages, missing acknowledgement, double completion, stale generation, cross-cache lease, ordinary-store-only publication, and generation wrap reject; cancellation defers physical reuse until acknowledgement; injected launch/event failure releases pins and leases; a live device probe reads exact block-table bytes; sanitizer/churn proves no race or leak. Files: maki/infer/kv-snapshot.f, focused host/fake-device/device tests, FILEMAP.md. Verify: common memory suites, CUDA lifecycle tests, paged-KV and decode integration, typed-local/trust/package/host/filemap/dot lints, device race probe, full native gate.
