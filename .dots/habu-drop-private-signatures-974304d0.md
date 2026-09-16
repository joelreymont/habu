---
title: Drop private signatures and symbols at capture
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T16:49:49.729611+03:00"
---

Problem: the checker user-signature store (USIGS-USER, 1.96 MB of the image as sparse runs) and the symbol tables (SYMS-BOOT, SYM-STR-BOOT, 571 KB) carry an entry for every one of the engine 15,470 words, but 8,022 are package-private and, once every captured package is sealed, no REPL source can name them, so their signatures and symbols are dead weight in every engine and every application image. Acceptance: at capture, after the seal, the signature and symbol entries of private words of sealed packages are removed (or never captured) with the tables compacted, keeping every entry the public surface, the keep-set and the checker own bookkeeping need; the checker still checks user code against every public word and refuses private names as undefined; engine-size.f reports the store and symbol bytes before and after; byte fixpoint; full gate and stripped-application suites green; Radar and Tender green on the result. Files: src/core/checker.f (capture-time compaction), src/habu/aot-capture.f, tools/engine-size.f. Verify: tools/engine-size.f; tools/native-build.f fixpoint; test/run.f; downstream suites. Depends: the seal child. Ownership: checker capture. Claim: unassigned. Parent: the ship-only-the-surface epic.
