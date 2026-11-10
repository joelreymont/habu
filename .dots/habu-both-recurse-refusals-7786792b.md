---
title: Both recurse refusals suggest the generic uncheckable rewrite
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T12:47:37.142867+02:00"
---

Problem: src/core/render.f UNCK-SUGGEST$ - the suggestion for a recurse with no declared effect and for a recurse in a does> clause is 'Rewrite with modeled words or isolate an audited primitive.', which fits neither. Reported by the recurse-in-does worker. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: each recurse reason's suggestion says its own fix (declare the effect; move the recursion into a named word). Files: src/core/render.f, test/signature-nesting.f. Verify: the two packets' suggestion fields. Depends: habu-recurse-in-a-d44e8ae6 landed (same words). Ownership: the checker's diagnostics. Claim: unassigned.
