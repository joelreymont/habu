---
title: Attribute the captured heap and drop sealed-dead records
status: open
priority: 2
issue-type: task
created-at: "2026-09-17T12:39:26.146860+03:00"
---

Problem: the captured heap, everything above the last declared cell, is 1241138 bytes in 58035 runs with an image cost of 1705418 bytes, a third of the 5112000-byte line-head engine (tools/engine-size.f 2026-09-17, shown as owner DONE because the heap has no owner). Nothing says which allocator owns it: checker declaration records, symbol records (SYMS-BOOT 113678 bytes plus SYM-STR-BOOT 237517), dictionary bodies, IR tables, hash tables. And after the internal-mark seal no source can name a sealed-internal word, yet the product image still carries its checker declaration and symbols; 4016 records (195772 code bytes) are unreachable from the engine-entry roots and only the public dictionary surface reaches them. Acceptance: (1) engine-size attributes heap bytes by allocating structure (each long-lived allocator marks its region or the capture records HERE marks per subsystem) and lists the top owners; (2) a measurement states how many bytes are declaration and symbol records of sealed-internal words; (3) the product image drops those records the way it drops their names, the whitebox image keeps them, with the measured size change stated, or the dot records the reason it cannot; byte fixpoint; test/run.f; whitebox suites through WHITEBOX-SUITE. Files: tools/engine-size.f, src/core/checker.f (SYMS-BOOT, declaration records), src/core/internal-mark.f, src/habu/aot-capture.f. Verify: engine-size; fixpoint; test/run.f. Depends: habu-strip-the-names-89d6524a for the sealed-internal set. Ownership: engine size. Claim: unassigned.
