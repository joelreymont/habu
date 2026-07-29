---
title: Package snap.f and build-fixpoint-test.f
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T22:50:34.751178+02:00"
---

Full context: follow-up from packaging snap-lib.f (agent pkglayout). src/habu/snap.f still defines global words (SNAP-RETIRE-GO and the driver tail) and test/../tools/build-fixpoint-test.f pins its emitted source string at tools/build-fixpoint-test.f:705, so renaming anything the driver calls cascades: SNAPGO kept its historic spelling inside package SNAP because a shorter tail (GO) would make snap.f a changed global definition and pull both files in. Do the cascade properly: give snap.f a package owner (file stem snap is a forbidden tail prefix, so SNAP-RETIRE-GO needs a new tail), update the pinned emitted-source string in build-fixpoint-test.f and package that file too, then respell SNAP:SNAPGO to a short non-redundant tail. Gates: package-diff-lint and typed-local-diff-lint exit 0 on the artifact, fixpoint rebuild, gate-stdlib red set unchanged.
