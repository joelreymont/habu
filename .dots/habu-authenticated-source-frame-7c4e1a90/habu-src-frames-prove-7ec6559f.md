---
title: "Source frames: prove image parity"
status: open
priority: 2
issue-type: task
created-at: "2026-07-22T12:22:25.519568+02:00"
blocks:
  - habu-src-frames-unwind-446e760c
---

Problem: the source-frame contract is incomplete if native, Gforth recovery, snapshot, ahead-of-time, replay, or fixpoint paths serialize addresses, generations, or ambient parser state differently. Acceptance: mirror the final explicit implementation layout and lifecycle byte-identically, clear process-local frame state before publication, and restore only canonical identities and no live frames. Repeated snapshot or replay produces the same logical frames independent of allocation order and rejects stale live-state capture. Files: bootstrap manifests and mirrors, snapshot/AOT/replay integration, parity tests, and public source-frame documentation. Verify: native versus recovery fixtures, snapshot round trip, AOT execution, replay re-interning, two fixpoint generations, and exact no-live-frame publication rejection. Depends: Source frames: unwind failures. Ownership: cross-image parity and persistence proof only. Claim: unassigned.
