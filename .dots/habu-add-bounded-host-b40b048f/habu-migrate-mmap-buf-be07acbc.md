---
title: Migrate mmap buffer to MEM spans
status: open
priority: 2
issue-type: task
created-at: "2026-07-13T16:45:13.645381+02:00"
blocks:
  - habu-freeze-transient-mem-1a69322a
---

Problem: an mmap-backed buffer exposes raw address and length without region, lifetime, alignment, or unmap ownership, permitting stale use and mismatched bounds. Fix: migrate one complete mmap owner to MEM typed allocation/borrow/subspan APIs and make unmap consume its owner; preserve OS error distinctions. Acceptance: use-after-unmap, wrong mapping index, extent mismatch, double unmap, misaligned access, and failure cleanup reject; valid first/last access and exact mapping behavior pass. Files: one mmap owner module plus focused test and TRUSTED.md only if a row is discharged. Verify: exact OS/memory tests, negative checker fixtures, trust/refine/typed-local lints, full native gate.
