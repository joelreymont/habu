---
title: A read of an empty return row is pinned at a later token
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T18:14:14.454666+02:00"
---

Problem: ': X ( -- ) r> drop ;' and ': X ( -- ) r@ drop ;' are refused E-REJECTED, fix_return_stack, pinned at 'drop' and not at the word that reads the return row. Found by the hunt of habu-a-r-under-2e10d914 on commit 9e0ea0ef (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/rstack-hunt.md, Out of scope); not yet reproduced by the lead. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Hunt first: where the return-row read (src/core/checker.f RSR> and the words beside it) fails and why the pin moves to the last token; whether 2r> and 2r@ do the same. Acceptance: r>, r@, 2r> and 2r@ on a return row that does not hold the cells are refused at that token with the reason. Files: src/core/checker.f; cases in test/compiler/input-underflow-refusals.f or test/diag-position-test.f, as the hunt places them. Verify: the reproducers under --load tools/check.f -- --json-errors and --load. Depends: habu-a-r-under-2e10d914 (its fix may move the return-row words). Ownership: the return-row read's refusal. Claim: unassigned.
