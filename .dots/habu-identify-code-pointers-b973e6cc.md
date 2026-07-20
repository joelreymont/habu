---
title: Identify code pointers by relocation metadata, not magnitude
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-20T09:43:12.162871+02:00\""
---

Prerequisite A for the direct-BL campaign (habu-aot-repl-bl-a71440da), and independently correct. CELL-TEXTPTR? (src/habu/aot-lib.f:99-100) decides whether a cell is a code/dict pointer by testing membership in the [RBASE-VA, RBASE-VA+REGION) window, and AOT-DATA-TEXTPTR? (:101+) scans raw AOT data cells with it. That is a MAGNITUDE HEURISTIC: it is only safe today because RBASE-VA is 12.9 GiB, an implausible value for user data. Any legitimate user datum in that window is silently misclassified as a pointer, and the heuristic blocks moving the code region (prerequisite B) because a near-text window overlaps ordinary integer magnitudes. Fix: record which cells are relocatable at the point the emitter/capture CREATES them - explicit relocation metadata carried in the AOT record - and make every consumer classify from that metadata, never from the value. Fail closed on any cell whose classification is not recorded; do not keep the window test as a fallback (that would preserve the defect). Red-first: a data cell whose VALUE lands inside the code window must be proven MISclassified by the current code and correctly classified after. Acceptance: no consumer reads pointer-ness from magnitude; AOT capture/boot-patch/closure paths all drive off metadata; existing AOT suites green; a regression pins the false-positive case. Territory: src/habu/aot-lib.f, aot-capture.f, aot-closure.f + AOT tests. Engine change -> CODELEN rows same-commit.

Claim: agent=relocmeta workspace=.jj-ws/fable-relocmeta machine=spark (owns src/habu/aot-lib.f aot-capture.f aot-closure.f + AOT tests; holds the CODELEN rows jointly with jitshare - coordinate at merge)
