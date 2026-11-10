---
title: A type mismatch under a deeper stack is classed as a surplus value
status: active
priority: 1
issue-type: task
created-at: "2026-11-10T15:15:08.167878+02:00"
---

Problem: since 7f6680a7 (Render a refusal's rows as its token saw them) a mismatched cell with cells below it prints an expected row holding only the token's own inputs beside the whole actual stack, and the depths differ, so the packet says repair_class remove_producer and 'Remove an extra producer or drop the surplus value.' where a type is wrong and nothing is surplus: ': SN-B2 ( bool -- ) dup IF 1 IF THEN THEN drop ;' prints 'expected: bool actual: bool n' (before: 'expected: bool bool actual: bool n'); ': F ( n bool -- ) ... ; ... 1 2 F' prints expected 'n bool' actual 'n n n'. Cases /Users/joel/Work/reviews/habu-10x-2026-11-10/cases8/rowclass (b2.f, b3.f, b4.f, badcond.f) under tools/check.f --json-errors and --load. Found landing habu-unck-is-set-88095289, whose 33rd-IF case pinned the old rows; its two assertions in test/signature-nesting.f (:217, :221) now pin only the token, code, verdict and actual row. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: a hunt names as path:line why the expected row at the pin lacks the cells below the token's inputs and what ROWS-CLASS and ROWS-SUGGEST compare; then expected and actual cover the same cells, the three programs are classed and suggested as type mismatches, and the 33rd-IF case pins both whole rows again. Files: src/core/render.f, src/core/checker.f, test/signature-nesting.f. Verify: the cases; a case that fails before. Depends: none; same words as habu-a-refusal-s-4e54728f. Ownership: the checker's diagnostics. Claim: agent=bob workspace=.jj-ws.noindex/habu-a-type-mismatch-0d7ded80
