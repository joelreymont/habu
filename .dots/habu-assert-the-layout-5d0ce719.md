---
title: "Assert the layout facts the AOT linker's crossings assume"
status: open
priority: 3
issue-type: task
created-at: "2026-09-18T04:50:00.628983+03:00"
---

Problem: src/habu/aot-closure.f DATA-PTR ( n -- ptr u8 ) and CODE-N ( ptr u8 -- n ) (habu-convert-the-hand-9b6f0d97, 2026-09-18) cross between the address and pointer domains by arithmetic on data-base and AOT-DBASE@, assuming data-base == DATA-VA and that emitted code is mapped above dbase@; both are load-time facts of the native engine the file already relied on (XTC-CELL, CELL-CODEPTR?, DATA-ADDRESS?) and were verified numerically on two engines, but nothing asserts them, so a layout change would produce wrong addresses silently instead of a refusal. Acceptance: one check at AOT-LINK's entry (data-base BYTE-VIEW's address equals DATA-VA VA>N; AOT-CP-N above AOT-DBASE-N) dying by name if violated, with a test that forges the mismatch through the linker's own test seam. Files: src/habu/aot-closure.f, test/compiler/. Verify: the test; test/gate-aot-positive.f; test/run.f. Depends: habu-convert-the-hand-9b6f0d97 landing. Ownership: AOT linker. Claim: unassigned.
