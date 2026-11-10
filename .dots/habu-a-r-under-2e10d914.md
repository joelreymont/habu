---
title: A >r under the declared inputs refuses with no reason
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T18:07:39.364806+02:00"
---

Problem: a return-stack transfer that takes more data cells than the definition has above its declared inputs is refused only at a later token with no reason. On master 951b3f90: ': X ( -- ) >r r> drop ;' is E-REJECTED, unknown_rejection, at 'drop'; ': X ( -- ) 1 2>r 2r> 2drop ;' and ': X ( -- ) 1 >r >r 2r> 2drop ;' are E-REJECTED at '2drop'. A data stack word in the same place ('1 swap') is E-INPUT-UNDERFLOW, supply_missing_input, at the word, needs 2, has 1. Found by the worker of habu-a-stack-word-5dd345f9 fix one (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/transport-fix.md, Out of scope): with that fix '[: ;] catch 2>r 2r> 2drop' (a proof beside the operand) is E-INPUT-UNDERFLOW at '2>r', and the same shape without a proof is still E-REJECTED. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Hunt first: which path checks >r, 2>r and n>r without a proof and why nothing measures the borrow (src/core/checker.f STEP-BORROWS?, STEP-UNDERFLOW!, XP-RS?, XPORT-APPLY). Acceptance: >r, 2>r and n>r that reach under the declared inputs give E-INPUT-UNDERFLOW, supply_missing_input, at that token with needs and has, with or without a proof among the operands; no accepted program is refused. Files: src/core/checker.f; cases in test/compiler/input-underflow-refusals.f. Verify: the three reproducers under --load tools/check.f -- --json-errors and --load. Depends: habu-a-stack-word-5dd345f9 fix one (hunt and fix are written on it). Ownership: the return-stack transfer's borrow. Claim: agent=bob workspace=.jj-ws.noindex/habu-a-r-under-2e10d914
