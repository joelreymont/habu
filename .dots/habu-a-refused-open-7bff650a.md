---
title: A refused open-width local capture does not say why
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T12:17:48.336714+02:00"
---

Problem: a local bound over a cell whose width is open is refused at :} (src/core/checker.f LOC-BIND, dot c2c817ab) with a message that names two types and no reason: 'expected: option<a> actual: option<a>' for a word's own declared variable, 'expected: a actual: option<b>' for the untyped form. An agent reading it cannot tell what to change. Cause: the refusal goes through U-FAIL and UF-CAPTURE, which carry a pair of types only; a reason needs a diagnostic kind (the lane worker: a constant MD-OPEN-WIDTH latched with MDIAG! before UF-CAPTURE, and four case arms in src/core/render.f: MDIAG-CODE$ reusing E-MISMATCH, the repair-class table fix_type, the suggestion table, MDIAG-REASON$). Acceptance: the refusal says the value's width is not known at the bind and what to do (bind it at a concrete type, or pass it through without a local); test/checker-soundness-suite.f OPEN-WIDTH-LOCALS asserts the text. Files: src/core/checker.f, src/core/render.f, test/checker-soundness-suite.f. Verify: engine build; checker-soundness-suite rc 0; the three refused programs under ~/.cache/tmp/carl-locw print the reason. Depends: c2c817ab (lands first). Ownership: the files above. Claim: unassigned.
