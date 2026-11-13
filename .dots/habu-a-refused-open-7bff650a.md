---
title: A refused open-width local capture does not say why
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T12:17:48.336714+02:00"
---

Problem: a local bound over a cell whose width is open is refused at ':}' (LOC-BIND, src/core/checker.f) with a message that names two types and no reason: 'expected: option<a> actual: option<a>' for a word's own declared variable, 'expected: a actual: option<b>' for the untyped form. An agent reading it cannot tell what to change. Cause: the refusal goes through U-FAIL and UF-CAPTURE, which carry a pair of types only; a reason needs a diagnostic kind (proposed: a constant MD-OPEN-WIDTH latched with MDIAG! before UF-CAPTURE, and four case arms in src/core/render.f: MDIAG-CODE$ reusing E-MISMATCH, the repair-class table fix_type, the suggestion table, MDIAG-REASON$). Acceptance: the refusal says the value's width is not known at the bind and what to do (bind it at a concrete type, or pass it through without a local); test/checker-soundness-suite.f OPEN-WIDTH-LOCALS (:339) asserts the text. Depends: none. Claim: unassigned.
