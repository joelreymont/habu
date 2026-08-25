---
title: Cancel provisional KV batch
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:38:16.930498+02:00"
blocks:
  - habu-describe-provisional-kv-ead10e30
---

Why: a provisional batch or ready descriptor must be releasable before the first device enqueue without publishing cache state. Result: CANCEL-BATCH and CANCEL-READY return cancelled(cache) or refused(cache,the-input-carrier,cancel-error), restoring every provisional page, reference, reservation, descriptor row, and private staging owner only on success. The existing monotonic HIGH-WATER retains any observed provisional occupancy. Remove APPEND-TOKEN and every public single-row append wrapper in the same cut; Package KV remains the sole cache-state mutation owner. This leaf contains no DEVRT type, launch, pending, DONE, QUIESCED, commit, post-enqueue cancel, or terminal batch identifier; the separate finalizer owns post-enqueue publication and rollback. Owner: pre-enqueue cancellation and obsolete append-surface deletion only. Production red: APPEND-TOKEN can still publish one sequence independently, and begun or described work has no total pre-enqueue cancellation surface. Acceptance: cancellation restores committed lengths, page tables, references, reservations, free-list membership, and descriptor ownership exactly while preserving only the specified HIGH-WATER increase; injected cleanup failure returns the exact cache and carrier without mutation; repeated, stale, and cross-cache carriers refuse; APPEND-TOKEN and every public wrapper are absent from source, checker, tests, and generated images; the open, add, and describe paths remain exact. Forbidden: public constructor, descriptor edit, DEVRT work, snapshot, lease, boolean sync flag, compatibility API, new metric, partial commit, or second policy. Smallest owning check: focused CANCEL-BATCH, CANCEL-READY, HIGH-WATER, and removed-surface cases through maki/infer/kv-cache-test.f. Claim: unassigned.
