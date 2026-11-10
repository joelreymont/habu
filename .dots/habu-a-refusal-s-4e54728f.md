---
title: "A refusal's inferred effect and return stack show bindings made after the failure"
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T12:47:37.174263+02:00"
---

Problem: src/core/render.f - after habu-refusal-rows-take-3a1d211f the expected, actual and declared rows are rendered as the refused token saw them, but inferred_effect and return_stack are still rendered from the live rows at the definition's end. Reported by the refusal-rows worker. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: every row a refusal prints is the row at the refused token. Files: src/core/render.f, src/core/checker.f. Verify: the lane's case.f against its cut.f for both fields. Depends: habu-refusal-rows-take-3a1d211f landed (same words). Ownership: the checker's diagnostics. Claim: agent=bob workspace=.jj-ws.noindex/habu-a-refusal-s-4e54728f

Acceptance case 2026-11-10 (/Users/joel/Work/reviews/habu-10x-2026-11-10/cases8/unseen-refold/cases e4-mismatch-then-push.f, e3, e1): ': X ( -- ) 1 IF THEN 2 ;' printed actual 'n n' at IF, where the stack holds one n; check it on the lane's engine before closing.

Review 2026-11-10 (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/pin-rows-review.md, probes /Users/joel/Work/reviews/habu-10x-2026-11-10/cases8/pin-review): REVISE. The 33 hunt pairs agree, but (1) a refusal raised while compiling (an undefined word, a stray then, a name imported with using) still prints the definition's end rows, because REFUSAL-TAKE's CAPTURE-CLEAR zeroes the pin; (2) each kind pins at a different point of applying its token (after it for a byte-pointer @ or !, LIN-CHECK and the other OK-only failures; part-way through a join), so the inferred row is before, during or after the token by kind; (3) the walk after a pin is quadratic (habu-a-refused-def-fe5ca1f0); (4) the SV-PIN cells are dead. Finding 2 overturns pinning at the moment of failure: the lane is redesigned before any fold. Not landed; workspace kept.
