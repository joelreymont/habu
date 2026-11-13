---
title: Delete the dead code the comment sweep found
status: open
priority: 3
issue-type: task
created-at: "2026-11-09T17:59:25.281823+02:00"
---

Problem: with the old builders gone (habu-retire-the-whitebox-2467e4f2), these words have no caller on the arm64 path, by name in src, tools and test: src/core/checker.f CHECKER-ASIG-* and ARM, DISARM (:25100), ARMED?; SOURCE-INPUT:CLEAR-OBSERVED (src/core/include.f:135, observer machinery, no caller since SOURCE-ROOT:RESET went); checker.f TRANSFER-CHECKED (:29498) and the words only it calls (TRANSFER-PUBLICATION, TRANSFER-ROWS, TRANSFER-LINKS), uncalled since CHECKER-REG:HANDOVER went, with the comments naming it (:29256, :32805); tools/hb-build-lib.f HBB-MAP-COPY (:130) and HBB-MAP-FILL (:129), and HBB-PATH-HAS-DQ? (:144) with its refusal (:211), whose reason went (the source path is no longer written into program text, and tools/hb-build-core.f ROW+ escapes rows); src/core/include.f PREFIX-REWIND (:963), uncalled since src/habu/prefix-rewind.f went, and PREFIX-CUT and PREFIX-EPOCH, which lose their writer with it; src/habu/habu1.f's (FLUSHTO)/(NUM) registration (:5670-5673), whose only stated reason is now the direct branch (unverified whether the linker needs it). No caller by name does not prove nothing reaches a word: an xt can sit in a cell or a branch in code, as the reset's earlier deletion showed. The x86-64/Wasm capture words this dot named moved to habu-delete-the-capture-ba24cffd.
Acceptance: each word has no caller in src tools test (rg) and nothing in the linked hb reaches it (no BL or B to it in the engine text or live region, no cell holding its xt), and it goes, with every word only it calls; hb rebuilds itself byte for byte.
Files: src/core/checker.f, src/core/include.f, src/habu/habu1.f, tools/hb-build-lib.f, src/habu/prims.f, src/habu/kernel-x64.f.
Verify: rg each name over src tools test -g '*.f'; the image scan on the linked hb; hb-build --engine gen 2 equals gen 3.
Depends: none (lands after arm64). Ownership: those files. Claim: unassigned.
