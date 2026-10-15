---
title: Remove unreachable dictionary-scan fallbacks
status: open
priority: 2
issue-type: task
created-at: "2026-10-15T17:16:30.774476+03:00"
---

Problem: five lookup legs scan the whole dictionary when the hash table is absent: habu1.f:4402, 5769, 5849 and habu2.f:9802, 9946. HIDXP-CELL is stored once (habu1.f:5452), LHIDXBUILD runs before every lookup, and LOAD-MAX 3/4 keeps the table from filling, so production never takes them. Only test pokes write 0 (test/outer-interpret.f:127,139,1167; test/engine-writers-child.f:124). An index defect would silently degrade to O(NDICT), about 1.3 s per start, instead of failing. Acceptance: the five legs deleted; a missing table cannot occur by construction or fails loudly where it is detected; the tests that zero HIDXP-CELL test the remaining behavior without faking an absent table. Files: src/habu/habu1.f, src/habu/habu2.f, test/outer-interpret.f, test/engine-writers-child.f. Verify: rebuild bin/hb with tools/native-build.f (docs/bootstrap.md); bin/hb --load test/run.f; tools/two-generation-build.f convergence (engine output changes). Depends: carl-final-core chain landed on master (it edits these files); start from master after it lands. Ownership: listed files. Claim: unassigned.
