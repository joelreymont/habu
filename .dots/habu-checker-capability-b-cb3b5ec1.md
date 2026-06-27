---
title: "Checker capability (b): shared-memory tile type"
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T18:10:55.977089+02:00"
---

Sub-dot of habu-checker-capability-typed (capability (a) checked counted loop landed: lib/ptx/tile-loop.f TILE-LOOP). Add a shared<t,r,c> (or tile<space-shared,...>) matrix-tile type distinct from span<space-global> + STAGE/SMEM-LOAD TRUSTED: ops, with the address-space rule that space-shared and space-global never unify. Empirically the checker already carries an address-space param (space-global); test whether space-shared is accepted as a distinct symbol without a checker.f change (probe like tile-loop). Files: new lib/ptx/tile-smem.f + tile-smem-test.f (positive) + a negative fixture (space-shared used where space-global required -> reject) + TRUSTED.md rows + ptx-stdlib wiring. Dep: parent habu-checker-capability-typed.
