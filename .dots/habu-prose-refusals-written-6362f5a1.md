---
title: Prose refusals written outside render.f print no suggestion
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T15:15:08.180127+02:00"
---

Problem: since habu-a-load-s-99fed2b4 a definition's prose refusal ends with its packet's suggestion, but refusals written by other code print none: the compiler's locals refusals (src/habu/definers.f :434 more than 64 locals, :440 a local name over 16 bytes; their checker arms LOCALBAD-PROSE kinds 1-2 at src/core/render.f :1417-1424 look unreachable under a plain load), tools/check.f's 'missing name after create' (tools/check-verify-core.f :423) and the load's 'hb: reader keyword needs a name'. Programs /Users/joel/Work/reviews/habu-10x-2026-11-10/cases8/prose-review/p (local.f, many.f, noname.f). Found by the review of habu-a-load-s-99fed2b4 (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/prose-review.md, separate issue a). Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: each of these lines says what to write, from the same text its packet carries where it has one; a census lists every prose refusal writer outside render.f with what it prints; a case each. Files: src/habu/definers.f, tools/check-verify-core.f, src/habu/outer.f, src/core/render.f. Verify: the three programs under --load and tools/check.f. Depends: habu-a-load-s-99fed2b4 (its second commit covers render.f's other writers). Ownership: the checker's diagnostics. Claim: unassigned.
