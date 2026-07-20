---
title: Reference model inputs without one-local-each in capture
status: open
priority: 1
issue-type: task
created-at: "2026-07-20T20:07:51.063522+02:00"
---

The genuine remaining wall to any-size stacks, found by the final derive wave (3a364dd9): the MODEL: translator emits ONE TYPED LOCAL per signature input + >V name in the compiled capture word, and the frozen engine caps a definition at 64 locals (verified: the 65th typed local rejects; a 4-block stack dies at its 65th local). A 164-input 12-block GPT needs ~200 locals - impossible regardless of table sizing; the old CAP-CAP=64 was THIS engine wall wearing a table costume. Every model-proportional TABLE now derives (the sizing program is complete); the input/name dimension is engine-bounded at ~3 blocks. Fix at the translation root: the capture translator references inputs/named values by SLOT INDEX into the bound input region instead of minting a local per name - emit indexed reads (the executor already binds slots by index; the accessor machinery exists) with the locals reserved for the running value and true temporaries. Behavior-neutral bar: every existing MODEL: compiles to the same captured plan (the buffer-capture two-pass makes the rewrite mechanical); the 64-local engine cap stays (it is a sane per-definition bound - the translator just stops burning one per input). Acceptance: the 12-block GPT-2-shaped stack (164 inputs) captures, builds, backward-builds, gradcheck-samples - the acceptance the derive program hands off; plus the 4-block repro that dies today loads after. Red-first: the 65th-local reject is the baseline. Territory: maki/cad.f translation emit (CAP-EMIT-* / PARSE-BODY name resolution), tests. All table prerequisites landed (3a364dd9 + the full stage-3 chain).
