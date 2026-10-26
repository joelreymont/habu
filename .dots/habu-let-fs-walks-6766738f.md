---
title: Let FS walks own a growing context
status: open
priority: 2
issue-type: task
created-at: "2026-10-27T21:31:32.842128+02:00"
---

WALK-FILES, WALK-FILES-IF and REMOVE-TREE stood on one static context of FS-MAX-DEPTH (32) frames: a tree deeper than 32 levels was refused with E-FS-DEPTH, a nested walk with E-FS-WALK-ACTIVE, and two tasks needed caller-sized WALK-FILES-IN / REMOVE-TREE-IN contexts of FS-WALK-BYTES. Acceptance: each walk and removal maps its own frames for the call, doubles them before a descent needs one more and releases them on return or throw; FS-MAX-DEPTH and the public words REMOVE-TREE-IN, WALK-FILES-IN, WALK-FILES-IF-IN, FS-WALK-BYTES, FS-WALK-CTX0, E-FS-DEPTH and E-FS-WALK-ACTIVE are gone, and their consumers migrate before this installs: Zen read-state.f, write.f, normalize.f and jobs/worker.f, Maki project-ops.f and Tender failures.f; the descriptor-relative removal re-reads a child name from its parent frame after growth; a callback walks or removes another tree and two tasks walk at once through plain WALK-FILES and REMOVE-TREE; a refused deep removal closes every descriptor; the gate-pool scratch-refusal row provokes its refusal with a locked directory. Verification: hb --load src/lib/fs-test.f, hb --load src/lib/fs-mutate-test.f and hb --load test/gate-pool-test.f pass.
