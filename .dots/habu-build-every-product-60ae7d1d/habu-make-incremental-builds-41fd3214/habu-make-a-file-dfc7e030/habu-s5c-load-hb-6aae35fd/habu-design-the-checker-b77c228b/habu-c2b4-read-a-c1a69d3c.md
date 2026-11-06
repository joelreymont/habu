---
title: "C2b4: read a definer's created effect from facts text"
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T22:04:00.367927+02:00"
blocks:
  - habu-c2b-read-facts-f698adb6
---

Problem: design r2 D2: a create ... does> definer's created effect (checker.f NORET-CREATES@ :16208, NORET.CREATES, read at DO-TOK; CTL-GENERATES marks it as a generates: row's) is a whole effect, so its tag nests an effect: creates-effect ( EFFECT ) TAGS. The facts parse cannot nest today: PSIG clobbers SGHASR and the PD-* parse state. Acceptance: the tag is written by a render word after the control tags, the facts parse reads the nested effect into its own record without disturbing the outer one, and a probe over every symbol with a created effect finds the nested effect read back the same (E-CONTENT-SAME?). Files: src/core/checker.f, src/core/render.f. Verify: the probe; bin/hb --load test/run.f -- engine checker-soundness. Depends: C2b1 (parallel with C2b2). Ownership: those files. Claim: unassigned.
