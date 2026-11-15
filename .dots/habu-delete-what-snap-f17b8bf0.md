---
title: Delete the snapshot format nothing writes
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T19:15:03.263744+02:00"
---

Problem: no build writes a snapshot image. Deleting snap-rebase removed the arm64 writer's relocation passes, and src/habu/snap-x64.f, the x86-64 writer, is required by no file. What only a snapshot reads still stands, and every remaining piece changes x86-64 files, so this waits for the x86-64 step:

- the x86-64 boot's snapshot restore: src/habu/boot-x64.f SNAP-CARRIER,, SNAP-REGION, and SNAP-FRAME,, the snapshot branch of SNAP-START,, src/habu/snap-decode-x64.f, snap-x64.f, SNAP-CELL and src/habu/cell-grid.f;
- SNAPSHOT-FORMAT, the `snapshot-format` primitive (prims.f, habu2.f BSNAPSHOTFORMAT, kernel-x64.f) and layout.f SNAP-MAGIC and SNAP-TRL-*.

The call and address maps and the x86-64 site rows go with habu-find-whether-anything-b457bf9c. The cost column of tools/data-table-census.f measures cell-grid.f's form, so it goes with it.

The comments that still describe a snapshot writer or loader go with the code they describe: layout.f's snapshot comments, genio-core.f, elaborate.f, dict.f, docs/xcomp.md "Live-region sites", and test/xt-cell-band-bad.f.

Acceptance: what only a snapshot reads is deleted, with the tests that only exercise it; whatever remains has a reader that acts on it; the chain build reaches its fixpoint and the full suite passes.

Depends: the x86-64 step.

Claim: unassigned.
