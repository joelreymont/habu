---
title: "An undefined word's suggestion says nothing an agent can write"
status: open
priority: 3
issue-type: task
created-at: "2026-11-10T14:34:09.321674+02:00"
---

Problem: src/core/render.f SUGGEST-TEXT (:1541-1562) answers 'Inspect the token, signature, and raw stack evidence.' for an undefined word (RF-UNDEF :1544) and for every kind it does not name (:1561), and TOP-UNDEF-DIAG (:2117) the same at top level: it names no fix, the defect habu-both-recurse-refusals-7786792b fixed for recurse; since habu-a-load-s-99fed2b4 the prose line prints it too. Found while briefing habu-a-load-s-99fed2b4. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: an undefined word's suggestion says what to write (define it before this use, qualify it as PKG:NAME or import its package with using), in a body and at top level; a census lists every kind that reaches the :1561 default, and each gets its own text or is shown unreachable; a test pins the undefined word's. Files: src/core/render.f, test/signature-nesting.f. Verify: an undefined word under --json-errors and --load. Depends: habu-a-load-s-99fed2b4 landed (same lines). Ownership: the checker's diagnostics. Claim: unassigned.
