---
title: Give the UWL arena a grow path
status: closed
priority: 2
issue-type: task
created-at: "2026-09-16T16:51:15.408307+03:00"
closed-at: "2026-09-30T17:30:00.000000+02:00"
close-reason: "Dropped: the UWL high-water mark is 48 against a capacity of 1,024, and overflow is a named refusal."
---

Problem: MAXUWL (src/core/checker.f) sizes five UWL arrays and is the only checker prefix table with no grow path: overflow is U-PUSH named refusal. Measured high-water is 12 in the self-build and 48 across every checker suite; it was 4096 and is 1024 after c439dabe, 21x the mark, and no test exercises the refusal at either size. Acceptance: UWL grows geometrically like TV/VREC/VNARG/SYM-STR (the ENSURE pattern), the init cap drops to about 128 with the mark in its comment, the refusal stays only for the absolute ceiling and a fixture exercises it; DP freed reported. Files: src/core/checker.f, test/. Verify: checker suites; tools/native-build.f fixpoint; test/run.f. Depends: none. Ownership: checker tables. Claim: unassigned.
