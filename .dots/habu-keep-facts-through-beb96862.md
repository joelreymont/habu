---
title: Keep facts through a declared pass-through
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T19:34:25.751223+01:00"
blocks:
  - habu-judge-a-left-ff49fd26
---

Problem: a declared quotation parameter handed back drops its stated facts: with ': PASSQ ( [ -- ] -- [ -- ] ) ;' the word ': USE ( -- ) [: parse-name drop ;] PASSQ KV ! ;' loads, so KV, a TYPED-VARIABLE of [ -- ], holds a quotation that parses. Acceptance: load and check reject it at PASSQ or at the store as they reject the store without PASSQ; a diag-position case fails without it. Depends: ff49fd26. Claim: unassigned.
