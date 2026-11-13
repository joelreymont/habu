---
title: Delete the snapshot format nothing writes
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T19:15:03.263744+02:00"
---

Problem: no build writes a snapshot image. Deleting snap-rebase removed the arm64 writer's relocation passes, and src/habu/snap-x64.f, the x86-64 writer, is required by no file. What only a snapshot reads still stands, and every remaining piece changes x86-64 files, so this waits for the x86-64 step:

- the x86-64 boot's snapshot restore: src/habu/boot-x64.f SNAP-CARRIER,, SNAP-REGION, and SNAP-FRAME,, the snapshot branch of SNAP-START,, src/habu/snap-decode-x64.f, snap-x64.f and SNAP-CELL;
- SNAPSHOT-FORMAT, the `snapshot-format` primitive (prims.f, habu2.f BSNAPSHOTFORMAT, kernel-x64.f) and layout.f SNAP-MAGIC and SNAP-TRL-*.

The call kind of the call-site tables is the same: publish.f callmap-set, habu1.f BCALLMAPSET and kernel-x64.f record SNAP-RELOC:SITE-CALL (layout.f:1311), and nothing reads it. Deleting it cascades through publish.f RELOC-EXTERNAL and linker.f HOST-CALLS. It also moves layout.f ADDRMAP-OFF, which makes the x86-64 row band smaller; size that band for the address map alone, with no kind byte. Both arches lose the call kind together.

The comments that still describe a snapshot writer or loader go with the code they describe: layout.f's snapshot comments, habu2.f:156, genio-core.f, elaborate.f, dict.f, docs/xcomp.md "Live-region sites", and test/xt-cell-band-bad.f.

Acceptance: what only a snapshot or the call kind reads is deleted, with the tests that only exercise it; whatever remains has a reader that acts on it; the chain build reaches its fixpoint and the full suite passes.

Depends: the x86-64 step.

Claim: unassigned.
