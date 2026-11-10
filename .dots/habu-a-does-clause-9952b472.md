---
title: A does> clause packet under evaluate-closed counts from the clause text
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T12:47:37.153720+02:00"
---

Problem: under evaluate-closed with DIAG-JSON! a refused does> clause's packet gives its position from the start of the clause text (line 1, column 18) and its definition_source includes the signature; tools/check.f gives the true position (line 2, column 48) and the body. Reproducers: /Users/joel/Work/reviews/habu-10x-2026-11-10/hunt-does-packet.md, last section. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: both paths give the same position and definition_source for a clause. Files: src/core/checker.f (CHECK-DOES-RUN, DOES-REPORT), src/core/render.f. Verify: underflow.f through both paths. Depends: habu-a-does-clause-d74786de landed (same words). Ownership: the checker's diagnostics. Claim: unassigned.
