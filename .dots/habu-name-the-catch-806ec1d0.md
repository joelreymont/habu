---
title: "Name the catch fix in a caught row's help"
status: open
priority: 2
issue-type: task
created-at: "2026-11-23T14:35:25.662811+01:00"
---

Problem: E-NONPARAMETRIC-EFFECT for a declared row that aliases R through a caught stale tail (test/catch-stale-suite.f D1 ( R [ R -- R ] -- n ) catch) helps 'run the quotation that yields the output row on every path, or declare the row the body leaves'; neither works there: the fix is reading the row where the code is 0, after code 0<> if code throw then. Acceptance: D1's help names that read; other E-NONPARAMETRIC-EFFECT helps unchanged. Depends: none. Open (review of lmrmpkys): the comment at src/core/checker.f NP-ALIAS-KIND restates its code; delete it. Claim: unassigned (its commit is on the parked stack lmrmpkys..xvroprym above tqsnwlwz, workspace .jj-ws.noindex/carl-infname).
