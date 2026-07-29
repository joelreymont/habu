---
title: Refresh checker citations in the models
status: open
priority: 3
issue-type: task
created-at: "2026-07-30T01:09:01.480451+02:00"
---

Full context: formal/Common/Effects.v and formal/Common/Control.v name the checker word behind every definition with a src/core/checker.f line reference, and a large number of those references have drifted. Measured while modelling MATCH's scrutinee pop: the header said MATCH lives at checker.f:8258-8352 and MATCH-SCRUT? was cited at 8236-8246, while the real lines are 8276-8413 and 8294-8302 - a consistent shift of about seventy lines through that whole region. The MATCH scrutinee leaf refreshed only the references it owned (the header's MATCH range and depth guard, the scrutinee pop, the diagnostic classifier, MATCH-FAM-TOK, and the QDEPTH note); everything else in both files is still as it was.

Why it matters: the reference is the only thing tying a model definition to the code it claims to model, so a reader checking faithfulness by eye is sent to the wrong word, and a reviewer cannot tell a stale citation from a wrong one.

What to do: walk every checker.f / type-family.f citation in both model files, resolve the named word through the shared source lexer rather than by eye, and correct the line numbers. Then consider whether the parity gate should hold them: the cases file already reads checker.f structurally, so a row that resolves a cited word name to its line and compares it with the model's citation would keep this from drifting again - that is the real fix and worth its own dot if it turns out to be more than a small walk.
