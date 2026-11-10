---
title: The card binds a local to res<n,n>, a family nothing defines
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T14:47:35.869675+02:00"
---

Problem: docs/forth-card.md section 2 'Bind multi-cell values whole' gives {: r:res<n,n> :} as its example; no family res exists under src (rg 'ENUM res|SUMTYPE res|STRUCTURE res' finds none), so the example does not load as written. Acceptance: the example names a family the tree defines (src/lib/adt/result.f result<n,n>) and loads. Files: docs/forth-card.md. Verify: load the example. Depends: none. Ownership: unassigned. Claim: unassigned.
