---
title: Type filesystem cleanup kind
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T21:13:07.747687+02:00"
blocks:
  - habu-pkg-remaining-30-99dbf693
---

lib/fs-mutate.f:12-14 defines cleanup action {file,dir,tree} as raw integers; FS-MUT-CLEANUP-KINDS is a generic cell array (:23-25) whose accessor returns ptr n (:61-64). FS-MUT-CLEANUP+ accepts n and performs three runtime inequality checks (:252-262); FS-MUT-CLEANUP-REMOVE rereads n and uses nested comparisons whose final else silently treats every unexpected value as file (:273-285). Kinds and path lengths are interchangeable to the checker because both columns expose n. After habu-pkg-remaining-30-99dbf693 gives fs-mutate its package owner, declare a private cleanup-kind ENUM with file ordinal zero, store the kind column in LAYOUT-BUFFER, make the internal append take that enum, and dispatch deletion through exhaustive MATCH after the symlink/existence policy checks. Delete raw constants, membership checks, generic kind pointers, and default fallthrough; public CLEANUP+/DIR+/TREE+ remain the only constructors. Preserve reverse cleanup order, symlink-first deletion, missing-path idempotence, error propagation, and exact destructive targets. Add checker negatives for n/length/foreign-enum stores, exhaustive tests for all kinds including symlink/file/dir/tree combinations, kind-column corruption impossibility, and CODELEN/JIT/DATA measurements with no growth. Files: lib/fs-mutate.f, fs-mutate-test.f. Coordinate but do not overlap the atomic-replace M2 slice. Ownership: cleanup-kind typing only.
