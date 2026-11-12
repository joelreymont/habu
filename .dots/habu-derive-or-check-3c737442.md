---
title: Derive or check the hand-kept counts
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T13:29:52.344324+02:00"
---

Problem: constants kept by hand to equal a count elsewhere drift silently until a bare throw: src/compiler/native/hir-word.f WORDS and PICK-CELLS must equal the rows and pick cells REGISTER-WORDS writes (the null stack's PTR>N made 100 against 99 and 17 suite tests printed only 'uncaught throw code -8283'); src/core/generated-declaration.f REASON-CAP 35 must equal the EXPLAIN calls across five files; tools/native-layout.f:45 $2D80, $2D88 and $2D98 must equal src/habu/layout.f's, unchecked. Acceptance: each count is computed from what it counts, or a build-time check names both numbers and the two places when they differ; nothing is left that only a runtime capacity throw catches. Depends: none. Claim: unassigned.
