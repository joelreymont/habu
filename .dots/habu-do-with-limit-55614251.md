---
title: ?do with limit below start wraps in the sig-pool scan
status: open
priority: 2
issue-type: task
created-at: "2026-08-17T17:55:40.147648+02:00"
---

Flagged by audit-close (2026-08-17): test/aot-sig-pool-suite.f:88's 'LINT-LEX:COUNT 2 - 0 ?do' wraps rather than skips when the lexed count is below 3, hanging or throwing E-VEC-BOUNDS. Latent - current fixtures never lex that short - found because the closer's own fixture needed the same guard. Fix: guard the bound (or use the tree's bounded-loop idiom if one exists); regression: a two-token fixture through the suite's own scanner. Check for siblings: rg for 'COUNT 2 - 0 ?do' and kin across test/ - the shape may recur.
