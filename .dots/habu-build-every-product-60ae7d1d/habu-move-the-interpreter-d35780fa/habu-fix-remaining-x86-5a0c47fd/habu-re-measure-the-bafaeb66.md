---
title: Re-measure the x86-64 tier-1 compile-floor budgets
status: closed
priority: 3
issue-type: task
created-at: "2026-11-01T20:01:33.602118+02:00"
closed-at: "2026-11-04T11:48:48.146195+02:00"
close-reason: "Census 2026-11-04, done: test/compile-floor-gate.f:50-55 now holds X64-T1 ARITH 4000, BRANCH 5500, SEARCH 52, FOLD, MOVE, LINES with fresh ranges (47a9d0d5)."
---

test/compile-floor-gate.f X64-T1-ARITH-BUDGET (3000, comment 1918..1958) and X64-T1-BRANCH-BUDGET (7100, comment 4524..4682) no longer match x86-64: master efef8267's x86 engine on joel@pulsar-ua-106 (load 0.4) reads tier-1 corpus leasts ARITH 2643..2695, BRANCH 3654..3739, SEARCH 35 (budget 38), FOLD 1266..1304 (1600), MOVE 140..144 (320), LINES 922..944 (1850); the T0 x86 stack reads the same. ARITH's budget is 1.1x its maximum, SEARCH's 1.09x and FOLD's 1.23x (a loaded run can go red), and MOVE and LINES sit above twice their minimum (a doubled cost stays green), against the file's rule (about 1.5x the range's maximum, below twice its minimum). The header's 'the other tier-1 budgets already cover it' is false for SEARCH and MOVE on x86. Fix: five standalone runs of test/compile-floor-gate.f on pulsar, X64-T1-* budgets for every tier-1 bench that the shared ones do not cover by the rule, header corrected. Found by the C4b fold-in C worker (jerry-t0x86floor). Claim: unassigned.
