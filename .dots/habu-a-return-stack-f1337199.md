---
title: A return-stack join refused at then says it does not balance at exit
status: open
priority: 3
issue-type: task
created-at: "2026-11-10T14:33:29.374995+02:00"
---

Problem: a return-row mismatch between the two paths that meet at 'then' is refused there with the reason 'return stack: the return row does not balance at exit' (src/core/render.f RS-REASON$ :1662-1665, the default arm; case /Users/joel/Work/reviews/habu-10x-2026-11-10/cases8/inferred/rsjoin-full.f): the token is not an exit and the reason does not say the paths differ. Found by the hunt for habu-a-refusal-s-4e54728f (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/inferred-hunt.md). Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: a return join's refusal has its own reason naming the join, and a suggestion that says what to write; a test pins rsjoin. Files: src/core/render.f, src/core/checker.f (RS-CAPTURE :4765, RSBAD), test/compiler/input-underflow-refusals.f. Verify: rsjoin-full.f. Depends: habu-a-refusal-s-4e54728f landed. Ownership: the checker's diagnostics. Claim: unassigned.
