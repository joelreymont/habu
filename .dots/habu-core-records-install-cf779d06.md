---
title: "Core records: install post-hook loader"
status: closed
priority: 1
issue-type: task
created-at: "2026-07-13T17:55:26.678113+02:00"
closed-at: "2026-07-15T21:27:08.365549+02:00"
close-reason: verified by focused, recovery, maki, ptx-stdlib, full native gates, and fresh destruction review
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

Claim: agent=core-records workspace=.jj-ws/habu-core-records-install-cf779d06
