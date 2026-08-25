---
title: Fix the string-as-family-id test calls
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T15:29:56.788337+02:00"
---

test/enum-decl-suite.f:84 and elsewhere call TWX-SUMV-ADD with a string where the family id belongs, binding fam to a string LENGTH. Harmless today only because SUMV-ADD does not validate fam — the rows are filed under a family that does not exist. Fix the call sites; then make SUMV-ADD validate the family id so the mistake is unwritable (checker-miss question: why did no rule catch a string length flowing into an id parameter?). Found by the CG-23 lane, 2026-08-05.
