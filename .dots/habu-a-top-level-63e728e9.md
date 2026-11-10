---
title: A top-level name only rendered text may define defers with no cause
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T15:22:55.089670+02:00"
---

Problem: the source pre-pass's TOP-DUE prints the general suggestion ('Rewrite with modeled words or isolate an audited primitive.') because TOP-ANSWER answers 3 both for a bound word whose effect names a type that may arrive and for a name nothing binds that a rendering statement may define (src/core/checker.f TOP-ANSWER, SYM-EFFECT-UNRESOLVED? and UNSEEN-COVERS? arms), and no commit of habu-too-deep-control-fa36caac owns it. The review's reading, inferred and not run: TOP-BINDING (already called in src/habu/verify-source.f by DECLARED-RENDER? and TOP-PARSER) answers zeroes when nothing binds the token, so TOP-RESOLVE can keep the cause (DW-RENDERED for an unbound name, DW-TYPE for a bound word with an unresolved effect) in a private variable beside TOP-DEFER-A/U, with no new public word. Found by the review of the deferral-cause commit (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/defer-cause-review.md, finding 3; programs d-stretch.f and u-top.f in /Users/joel/Work/reviews/habu-10x-2026-11-10/cases8/defer). Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: d-stretch.f's MADE prints the rendered-name text and u-top.f's T the type text; test/diag-position-test.f's top-level rendered-name case pins the text. Files: src/habu/verify-source.f, test/diag-position-test.f. Verify: the two programs; the case fails before. Depends: habu-too-deep-control-fa36caac commit one landed (same words). Ownership: the source pre-pass. Claim: unassigned.
