---
title: lib delete candidates
status: open
priority: 3
issue-type: task
created-at: "2026-08-22T22:38:25.972024+02:00"
---

Problem: zero/test-only consumers measured by rg: BUILD:CHECK + BUILD-CHECK-RAW + STEP record (lib/build.f:30-148,152-207; CHECK slices definitions by the first ':' and ';' bytes, broken by comments/locals/quotations; 0 consumers); RC-returning capture family; PROCESS-TRACE hooks (2 test files); tile-v4a.f .V4 family; ad-saved.f stubs; lib/ptx/ir.f PTXIR (0 outside lib/ptx) and ad-ir.f; launch.f PTX-LAUNCH-POSITIVE; HM: (hashmap.f, 0); MAP-* (map.f, one example); BOX-* (layout/box.f, 2 tests); TBL: (table.f, 0 - only Reloc.v text); RENDER: private CSV/JSON toolkit render.f:558-627; TIME: (2 consumers, wraps primitives); source.f INSERT-BEFORE-FINAL-LINE/CONCAT-FILES/WRITE-SOURCE-LIST/SOURCE-FILE-LINES (0); fs.f FILE-META/READ-LINK, fs-mutate.f MKDIR-MODE/MAKE-TEMP-DIR/COPY-FILE (8 KiB cap), string.f STR-COUNT, array.f EVEN?/A-MAX-INDEX/A-COUNT-EVEN/A-RUNMAX!/A-PREFIX-SUM!/A-SCAN!/A-SCAN1!/MIRROR-INDEX; FS:WRITABLE-ROOT? package (fold into fs.f); MMAP-TEST: (move beside its 2 maki tests). Acceptance: each deleted or its consumer named, counts re-measured in the commit. Files: lib/. Verify: full gate. Depends: none. Ownership: lib. Claim: unassigned.
