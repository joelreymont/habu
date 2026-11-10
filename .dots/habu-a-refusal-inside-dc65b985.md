---
title: "A refusal inside a quotation literal pairs the definition's inputs with the literal's rows"
status: open
priority: 3
issue-type: task
created-at: "2026-11-10T14:33:29.398347+02:00"
---

Problem: for a refusal inside a quotation literal, src/core/render.f INFERRED-EFFECT (:936) prints the definition's declared inputs against the literal's own rows (/Users/joel/Work/reviews/habu-10x-2026-11-10/cases8/inferred/quot-*.f, qrs-*.f: 'n -- n [ a -- | -- a ]'), so inferred_effect is the effect of neither the definition nor the literal. Found by the hunt for habu-a-refusal-s-4e54728f (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/inferred-hunt.md). Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: the packet's inferred_effect inside a literal is one of the two, stated in docs/forth.md beside the field, and a test pins it. Files: src/core/render.f, docs/forth.md, test/compiler/input-underflow-refusals.f. Verify: quot and qrs. Depends: habu-a-refusal-s-4e54728f landed. Ownership: the checker's diagnostics. Claim: unassigned.
