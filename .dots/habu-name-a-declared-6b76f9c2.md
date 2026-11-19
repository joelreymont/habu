---
title: Name a declared row in a narrowed-quotation rejection
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T12:37:29.121985+01:00"
---

Problem: test/engine-suite.f CBAD-QW-ROW ( [ R -- R ] n -- ) is rejected with = expected: [ A -- A ] and = found: [ B -- B ]: the carried quotation's tail is bound to the word's declared row R (RAN-KEPT?'s TAIL-OPEN? 0= branch, src/core/checker.f), but src/core/render.f RROW-LETTER prints a fresh letter, so the text never says the row is the declared R. Acceptance: that rejection names R in its expected line; a test pins the text and fails before. Depends: none. Claim: unassigned.
