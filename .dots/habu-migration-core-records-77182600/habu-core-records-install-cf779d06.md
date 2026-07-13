---
title: "Core records: install post-hook loader"
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T17:55:26.678113+02:00"
blocks:
  - habu-core-records-remove-31f84baf
  - habu-core-records-remove-0d8ff4e2
---

Own src/habu/habu2.f, bootstrap/cg/forth.fs, and tools/bootstrap.sh load order. Establish utilities -> checker explicit layouts -> lower-cert base -> type-schema explicit layouts -> type-family explicit layouts -> render -> check-hook -> unified STRUCTURE/ENUM -> remaining core in both native and recovery paths. Move CELL to the earliest bootstrap constant owner and PTR-VARIABLE to an independent checked owner. Remove structures.f/effects from the pre-checker prefix. Prove exact native/recovery offset, stride, alignment, pointer-role, load-order, and fixpoint parity; no cold parser, descriptor arena, adoption, or bootstrap-only declaration semantics.
