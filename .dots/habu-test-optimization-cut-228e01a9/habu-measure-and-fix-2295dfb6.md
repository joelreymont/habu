---
title: Measure and fix tier-1 cost of deep loop nests
status: closed
priority: 3
issue-type: task
created-at: "\"2026-10-01T04:19:38.363353+02:00\""
closed-at: "2026-10-01T17:06:11.536468+02:00"
close-reason: Fixed by nwwmxyxm a4b3c694 (review 94 ACCEPT)
---

Problem: 24 nested `?do … +loop` levels at tier 1 (`1 set-tier`) do not finish within 120 s on d40cc36d or on change yrykozmu, while 16 levels run 2^16 turns at both tiers in well under a second (test/runtime-regression-test.f GE-DO-DEPTH-CAP). 2^24 turns of an empty body is not two minutes of work, so the time is likely compile-time growth in the tier-1 elaborator or fold with nesting depth; not measured. Acceptance: time compile and run separately for nesting depths 8 to 24 at tier 1, name the step whose cost grows faster than linearly in depth (with the measurement), and fix it at that layer so 24 levels compile in time linear or near-linear in depth; or show the time is the turns themselves and say so. A test pins the depth the fix makes affordable. Files: src/compiler/native/ (where the measurement points). Verify: the native loop suites, the new row. Depends: none. Ownership: tier-1 cost of deep loop nests.
