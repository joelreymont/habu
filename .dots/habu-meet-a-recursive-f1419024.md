---
title: "Meet a recursive word's kept facts at recurse"
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T19:52:16.383205+01:00"
---

Problem: recurse drops a quotation's facts when the word keeps it later in the walk: with 'TYPED-VARIABLE KV [ -- ]' the word ': P ( [ -- ] n -- ) 0= IF drop [: parse-name drop ;] 1 recurse ELSE KV ! THEN ;' loads and checks with rc 0, so KV holds a quotation that parses; with the store before the recurse it is rejected. RECURSE instantiates the effect from before the body, and RECUR-CHECK (src/core/checker.f:22-26) meets kinds and restrictions afterwards, never kept facts (inferred, not tested). Acceptance: load and check reject P; a diag-position case fails without it; the pass-through forms 51recps.f and 62recps2.f in .out/designs/pass-facts-cases (root checkout) are rejected too. Depends: none. Claim: unassigned.
