---
title: Raise REQUIRE-MAX above the maki suite inventory
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-20T09:52:29.966383+02:00\""
---

Proven at the attn landing (2026-07-20): REQUIRE-MAX = $100 (src/core/include.f:9) caps the per-image require inventory at 256, and maki/test.f's full-image inventory now sits EXACTLY at the cap - appending a single empty probe file to the unmodified inventory dies 'require: too many files' (proven by the attn lane against pristine master). The gate is unaffected (it spawns the slices from test/run-lib.f), but the standalone full-image 'bin/hb --load maki/test.f' run can accept no new suite, and every future maki lane adds suites. Fix: raise the constant (e.g. $100 -> $200), keeping the fail-closed named die at the new cap - the fixed-cap+named-die idiom is correct, the cap value is just outgrown (same shape as INCLUDE-MAX-DEPTH's 2026-07-15 bump one line above). Red-first: prove the 257-file inventory dies at the current cap, passes after, and a NEW-cap+1 inventory still dies named. Regression pins the cap behavior. Measure DATA/CODELEN impact (the require table is sized by the constant) and update size rows same-commit. Fixpoint x2, full cold gate. Territory: src/core/include.f + a require-cap regression test.

Claim: agent=reqmax workspace=.jj-ws/fable-reqmax machine=spark (owns src/core/include.f + require-cap regression; engine lane - CODELEN rows same-commit)
