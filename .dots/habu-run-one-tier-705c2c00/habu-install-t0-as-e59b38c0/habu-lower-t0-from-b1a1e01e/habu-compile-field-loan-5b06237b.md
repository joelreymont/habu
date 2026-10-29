---
title: Compile field-loan, init and stow sites in T0
status: open
priority: 2
issue-type: task
created-at: "2026-10-29T17:41:00.477438+02:00"
---

Tier 1 pushes proved facts before these calls: a field-loan site pushes off and extent (elaborate.f:3581-3585, SITE-FIELD), an init site pushes width, bytes and align (elaborate.f:3564, SITE-INIT), and C2 stow has its own path. T0 compiles init and stow sites as plain calls (wrong arguments) and, after S2A, refuses field-loan sites (fold-in F1). T0 must lower all three as tier 1 does before tier 1 retires; test on test/c2-field-loan-e2e.f's image. Found by the S2A-2 review (/tmp/jerry-big/s2a-foldins.md F1).
