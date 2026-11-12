---
title: PTR>N refuses a pointer to a linear non-family type
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T15:05:58.401347+02:00"
---

Problem: `( ptr bpa-tok -- n ) PTR>N` (bpa-tok a DEFLINEAR type, not a family; fixture in test/compiler/base-pointer-arith-refusals.f) is refused rc 70 'Inspect the token…' by the installed engine and by the nominal-2 engine, though PTR>N reads nothing through the pointer and accepts a pointer to a linear family. Either the refusal is the rule (then a refusal test and a forth.md rule say why) or PE-ADDR!'s pointee must admit it. Acceptance: one of the two holds, with a test case. Depends: none. Claim: unassigned.
