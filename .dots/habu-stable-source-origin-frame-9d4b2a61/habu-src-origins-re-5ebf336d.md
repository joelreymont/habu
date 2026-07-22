---
title: "Source origins: re-intern images"
status: open
priority: 2
issue-type: task
created-at: "2026-07-22T12:22:52.153351+02:00"
blocks:
  - habu-src-origins-rollback-1db4a907
  - habu-src-origins-reflect-7190b5bf
  - habu-src-frames-prove-7ec6559f
---

Problem: snapshot, ahead-of-time, replay, and fixpoint images cannot persist process-local origin handles or allocation order. Acceptance: serialize canonical origin identity and diagnostic metadata, then re-intern records on restore so raw indices and allocation order never become identity. Reject missing parent, cyclic chain, digest mismatch, duplicate-conflicting row, and live provisional capture. Repeated replay is byte-stable and preserves exact diagnostics without changing semantic hashes. Files: origin snapshot/AOT/replay rows, bootstrap manifests, parity fixtures, and origin documentation. Verify: reordered allocation, round trip, nested chains, corrupted rows, two fixpoint generations, and native/recovery parity. Depends: Source origins: rollback stale records and Source frames: prove image parity. Ownership: origin persistence and re-interning only. Claim: unassigned.
