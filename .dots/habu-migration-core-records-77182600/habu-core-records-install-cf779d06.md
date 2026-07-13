---
title: "Core records: install post-hook loader"
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T17:55:26.678113+02:00"
blocks:
  - habu-core-records-remove-31f84baf
  - habu-core-records-remove-0d8ff4e2
  - habu-core-bootstrap-isolate-45800fec
  - habu-owner-seal-persist-1f23e205
---

Own src/habu/habu2.f, bootstrap/cg/forth.fs, and tools/bootstrap.sh load order.
Establish utilities -> checker explicit layouts -> lower-cert base -> type-schema
explicit layouts -> type-family explicit layouts -> render -> check-hook ->
post-hook declaration seam -> remaining core in both native and recovery paths.
Consume the independent CELL and PTR-VARIABLE owners and remove structures.f
and structures-effects.f from the pre-checker prefix. Prove exact
native/recovery offset, stride, alignment, pointer-role, load-order, and
fixpoint parity; no cold parser, descriptor arena, adoption, or bootstrap-only
declaration semantics. `src/core/structure-decl.f` and
`src/core/enum-decl.f` do not exist yet: this dot establishes their sole
post-hook insertion boundary but does not create, load, or implement them.
Their implementation dots install them at that seam after the shared field
schema lands. The owner-persistence blocker is a file-ownership serialization
edge for `habu2.f` and the Gforth mirror, not a semantic dependency.
