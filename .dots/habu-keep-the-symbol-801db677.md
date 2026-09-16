---
title: Keep the symbol table from baking a second copy
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T16:51:15.410313+03:00"
---

Problem: SYM-CAP-INIT (src/core/checker.f) is 16,384 rows and the boot prefix fills 15,726 (96 percent); it already grows to 32,768 under the AOT suites. When the prefix itself crosses the cap, the table grows during the build, the capture bakes the used span into fresh DATA and the dead boot buffer stays, so DP jumps about 1.3 MB and the image carries both. The same shape threatens SYM-STR-INIT ($60000 after c439dabe, mark 235,830). Acceptance: either the capture reclaims a grown table into a single right-sized copy (the boot buffer is dropped) or the cap is raised with the mark and a build-time check refuses a prefix that crosses it by name; a fixture proves no table is captured twice (tools/engine-size.f or tools/data-table-census.f owner rows show one copy). Files: src/core/checker.f, src/habu/aot-capture.f, test/. Verify: engine-size; tools/native-build.f fixpoint; test/run.f. Depends: none. Ownership: checker tables and capture. Claim: unassigned.
