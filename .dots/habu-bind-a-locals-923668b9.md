---
title: Bind a locals group inside a quotation body
status: open
priority: 2
issue-type: task
created-at: "2026-08-10T13:05:45.658071+02:00"
---

LGB is a running cursor the enclosing walk advances past each locals group's closer; a walk that SKIPS a quotation body advances none of the body's groups, so elaborate.f QLOCALS-CK refuses the shape by name (S1 boundary, measured population ~0: 88% of the tree's 268 bodies are a single token). Lift: make the group a lookup keyed by its closer token instead of a running count, so body and enclosing groups resolve independently of walk order. Acceptance: a body with a {: :} group compiles and executes; the enclosing word's own groups unaffected; QLOCALS-CK's fixture inverts. Files: src/compiler/native/elaborate.f. Depends: habu-compile-a-quotation-04341c80.
