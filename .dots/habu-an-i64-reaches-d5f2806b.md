---
title: An i64 reaches a u8 input through any word declared n
status: open
priority: 1
issue-type: task
created-at: "2026-11-10T13:32:12.588988+02:00"
---

Problem: ': ID ( n -- n ) ;  : U8C ( u8 -- ) drop ;  : X ( i64 -- ) ID U8C ;' is accepted: n passes integers in both directions (src/core/checker.f INT-WIDENS? :2378-2379; docs/forth-card.md:111-116 says so), so any word declared n launders a wide value into a narrower declared input, which conflicts with the card's statement that i64 needs an explicit conversion. Found by the hunt for habu-the-checker-accepts-fbca7120 (program b3 in /Users/joel/Work/reviews/habu-10x-2026-11-10/cases8/join). Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". This is a rule of the language, not a defect of one word: what n means against a fixed-width integer is Joel's to settle before any design. Acceptance: Joel's ruling recorded here; then a design. Files: src/core/checker.f, docs/forth-card.md. Verify: b3. Depends: Joel's ruling. Ownership: the checker's integer rule. Claim: unassigned.

Ruling 2026-11-10 (Joel chose "Narrowing converts"): any integer passes to n; n into a fixed-width input needs a conversion, fitting literals excepted. Next: a design, starting with a census of the tree definitions the rule refuses.

Scope 2026-11-10 (review of habu-the-checker-accepts-fbca7120, /Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/join-tagcheck-review.md): the design also covers the join and the loop, which the ruling decides. A join of n with a fixed-width t gives n (src/core/checker.f ROW-JOIN-PREP :4188, CON-OK? :2403-2404, CF-JOIN-D); at a loop, entry n with back edge t is sound and entry t with back edge n is the narrowing (CF-CARRIED=). Acceptance cases, all in /Users/joel/Work/reviews/habu-10x-2026-11-10/cases8/join-review with both engines' results in table.txt, types.txt and c.txt: the seven programs of habu-the-checker-accepts-fbca7120 refused; b/na7.f refused; c/rs3.f (the return-row walk) and d/c2body.f (a u8 reaching an addr input inside a loop body) decided and pinned; d/c3.f and c3b.f (300 and an i64 reaching ( u8 -- ) through a loop) refused; a literal that fits accepted and 300 into u8 refused (no literal-fit check exists yet). The unlanded commit ulokwsyz 95d8e7a8 in .jj-ws.noindex/bob-join holds the withdrawn fix and its test.

Design 2026-11-10 (/Users/joel/Work/reviews/habu-10x-2026-11-10/designs/n-rule-design.md): with Joel before any worker. Its findings outside the rule: a raw variable carries an i64 into a u8 word (the open raw-cell rule, checker.f :3458-3464); c, declares n while c! declares u8 (prims.f :590, :562); the comment at checker.f :7899 states c@ answers n, against prims.f :561; the census report's 'c@ yields n' is wrong.
