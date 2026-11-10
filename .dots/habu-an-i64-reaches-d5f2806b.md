---
title: An i64 reaches a u8 input through any word declared n
status: open
priority: 1
issue-type: task
created-at: "2026-11-10T13:32:12.588988+02:00"
---

Problem: ': ID ( n -- n ) ;  : U8C ( u8 -- ) drop ;  : X ( i64 -- ) ID U8C ;' is accepted: n passes integers in both directions (src/core/checker.f INT-WIDENS? :2378-2379; docs/forth-card.md:111-116 says so), so any word declared n launders a wide value into a narrower declared input, which conflicts with the card's statement that i64 needs an explicit conversion. Found by the hunt for habu-the-checker-accepts-fbca7120 (program b3 in /Users/joel/Work/reviews/habu-10x-2026-11-10/cases8/join). Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". This is a rule of the language, not a defect of one word: what n means against a fixed-width integer is Joel's to settle before any design. Acceptance: Joel's ruling recorded here; then a design. Files: src/core/checker.f, docs/forth-card.md. Verify: b3. Depends: Joel's ruling. Ownership: the checker's integer rule. Claim: unassigned.

Ruling 2026-11-10 (Joel chose "Narrowing converts"): any integer passes to n; n into a fixed-width input needs a conversion, fitting literals excepted. Next: a design, starting with a census of the tree definitions the rule refuses.
