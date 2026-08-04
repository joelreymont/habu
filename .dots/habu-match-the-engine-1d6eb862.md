---
title: Match the engine on tight loop bytes
status: open
priority: 2
issue-type: task
created-at: "2026-08-04T18:09:32.550849+02:00"
---

Goal clause violation: CODEGEN-CORPUS2:T-RES-WALK is the one row of all 38 where the new column loses — 76 bytes vs the engine's 36 (2.1x) for the small begin/while/repeat walker, cost matched. The if-converter cannot help (a loop backedge is not a selection). Diagnose from the emitted code (NWALK/NBR readers + tools/imagedisasm if present): where do the extra 10 instructions come from — block-argument moves the engine avoids by keeping the loop value in one register, a loop header/join shape that duplicates the test, entry/exit crossings, or edge copies the allocator's coalescing misses? Then fix the mechanism, not the row: likely candidates are backedge argument coalescing (the loop-carried value should stay in one register across the backedge so the edge copies vanish), loop-rotation (test at the bottom so the header is not duplicated), or fallthrough layout for the loop exit. Acceptance: T-RES-WALK new bytes <= 36 with cost matched-or-better and answers identical; no other row regresses in either column (tools/codegen-compare.f 0 findings and the full byte table win-or-match); the mechanism stated with before/after disassembly in the report. Files: src/compiler/native/select.f (loop/block shape), regalloc.f (edge coalescing), emit.f (layout/fallthrough). This is the last row between the branch and the goal's win-or-match clause.
