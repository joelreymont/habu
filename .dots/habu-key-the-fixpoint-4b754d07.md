---
title: Key the fixpoint stamp on the boot prefix
status: open
priority: 2
issue-type: task
created-at: "2026-08-12T09:29:59.549091+02:00"
---

Found by the prewindow lane (2026-08-12, probe with revert in the report): a +4096 edit to src/core/internal-mark.f moves EVERY booted engine's DP by 4096 bytes and install reports 'fixpoint: cached' - the stamp (BF-STAMP-KEY!) hashes the engine, the chain closure, and stage2/stdin sources, and covers boot-prefix files only where they happen to overlap those. Harmless today because nothing pre-window is carried (EM-AOT-RELOC-DATA self-corrects the window delta at boot); becomes a silent-wrong-address gap the moment anything bakes a prefix-relative constant. Fix: fold the boot-prefix file list (the PFX-LOAD rows' closure) into the stamp preimage the way the chain closure was folded (STAMP-KEY precedent, keyfix landing), must-fail-first: the internal-mark probe forces a rebuild after, still-cached for out-of-prefix edits. Interacts with habu-boot-pin-bake-8b284046. Files: tools/build-fixpoint.f. Depends: none.
