---
title: Prove safe FS milestone
status: open
priority: 1
issue-type: task
created-at: "2026-07-17T13:09:24.446573+02:00"
blocks:
  - habu-fs-make-atomic-61537711
---

Fan-in: habu-fs-checked-no-7b20610f and habu-fs-make-atomic-61537711. On one exact rebased tree, independently review syscall constants/effects, final-component no-follow behavior, descriptor identity, structured multi-error outcomes, temp-inode exclusivity, fsync/rename ordering, and failure atomicity. Run macOS/Linux parity fixtures, engine/stdlib/seal/bootstrap/fixpoint, typed-local, trust, host, filemap, dot, and full gates. Fast-forward and push green master, then close M2 leaves and milestone; later milestones consume only that master API.
