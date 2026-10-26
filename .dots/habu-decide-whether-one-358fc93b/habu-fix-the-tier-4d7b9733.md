---
title: Fix the tier 0 CAST 1+ miscompile
status: closed
priority: 1
issue-type: task
created-at: "2026-10-26T11:28:18.197192+02:00"
closed-at: "2026-10-26T11:48:23.545340+02:00"
close-reason: Fold only integer constants in tier 0 unary ops
---

Repro: 0 set-tier  CAST: FB ( r -- n )  : P ( -- n ) 1.0 2.0 f+ FB 1+ ;  P .  prints 4611686018427387904 (a float's bits reinterpreted wrongly; expected the cast's integer). Fix: VUN-ENTRY (src/habu/jit.f ~925) folds only tag 1; tags 2/3 take the spill path. Regression test through the real load path. Tier 1 refuses the same word: a separate tier 1 defect, dot it, no tier 1 fix. Experiment order (Joel 2026-10-26): fixes first.

Lane: jerry, worker in .jj-ws.noindex/jerry-cast; handoff ~/.cache/tmp/jerry-cast-handoff.md. Claim: jerry.

Done: tier 0 VUN-ENTRY folds only tag 1; tags 2/3 fall through to the primitive call (LVSPILL). test/runtime-regression-test.f GE-VUN-FLOAT pins it (failed before: 4611686018427387904). Full suite 501/501 on the candidate. No two-generation check: no image contains tier 0 code (src/habu/jit.f header).
