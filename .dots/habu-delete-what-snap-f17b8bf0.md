---
title: Delete the snapshot format nothing writes
status: active
priority: 2
issue-type: task
created-at: "2026-11-12T19:15:03.263744+02:00"
---

Problem: no build writes a snapshot image now. Deleting snap-rebase removed the arm64 writer's relocation passes. src/habu/snap-x64.f, the x86-64 writer, is required by no file (its header names a snap-lib.f that does not exist).

What only a snapshot reads still stands:
- SNAPSHOT-FORMAT (src/habu/snapshot-format.f) and the `snapshot-format` primitive (src/habu/prims.f:507, habu2.f BSNAPSHOTFORMAT, kernel-x64.f:4429);
- the snapshot trailer (src/habu/layout.f SNAP-TRL-*);
- the x86-64 boot's snapshot restore (src/habu/boot-x64.f, src/habu/snap-decode-x64.f);
- tools/imgdump.f LOAD-SNAPSHOT and HAS-SNAP branches, and src/habu/layout.f RBASE-VA, whose writer was the deleted EMIT-ADDRS pass.

The call-site tables' call kind is the same kind of leftover. publish.f callmap-set, habu1.f BCALLMAPSET and kernel-x64.f:3528 and :5847 record SITE-CALL, and so does link-x64.f:540. Only src/habu/sites.f yields it, to aot-capture.f:1460 and :1505, which drop it. Both arches keep a piece of the shared layer or lose it together.

The XTBAND-RC comment in layout.f names a writer's canonicalise and a loader's relocate.

Acceptance: what only a snapshot or the call kind reads is deleted, with the tests that only exercise it; whatever remains has a reader that acts on it; the chain build reaches its fixpoint and the full suite passes.

Depends: none.

Claim: agent=worker workspace=.jj-ws.noindex/snap-format
