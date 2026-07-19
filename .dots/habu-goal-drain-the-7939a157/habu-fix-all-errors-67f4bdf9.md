---
title: Fix --all-errors double-registering declared families
status: closed
priority: 2
issue-type: task
created-at: "\"2026-07-19T06:16:46.631507+02:00\""
closed-at: "2026-07-19T06:49:59.410065+02:00"
---

Found during DEFTYPE stage B (out of that change's scope, pre-existing): the full check-CLI --all-errors path runs the nominal/definer pass and THEN the all-errors redrive, registering a declared family twice, so ANY source with a NOMINAL: declaration (and plausibly SUMTYPE/TYPEFAMILY - probe) rejects with E-DUPLICATE-DEFINITION under --all-errors while plain preverify and the isolated CHECK-ALL-ERRORS-FILE path are fine. The gate never exercises --all-errors with a family declaration, which is why it survives - add that missing coverage as part of the fix (a green --all-errors case with a NOMINAL: declaration and a red-first pin of the current double-registration). Root-cause the pass sequencing (who registers, who redrives) rather than suppressing the duplicate error.
