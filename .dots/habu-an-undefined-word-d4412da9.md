---
title: "An undefined word's suggestion says nothing an agent can write"
status: open
priority: 3
issue-type: task
created-at: "2026-11-10T14:34:09.321674+02:00"
---

Problem: src/core/render.f SUGGEST-TEXT (:1625-1646) answers 'Inspect the token, signature, and raw stack evidence.' for an undefined word (RF-UNDEF :1628) and for every kind it does not name (:1645), and TOP-UNDEF-DIAG (:2209) the same at top level: it names no fix, and the prose line prints it too. Likewise MD-SUGGEST$ has no arm for MD-CON-TRUNC, so it prints the MATCH-form suggestion (program /home/joel/Work/reviews/habu-10x-2026-11-10/cases8/prose-review/p/contrunc.f), and MD-CON-FAM, MD-FAM-KIND and MD-CON-KIND (:1257-1259) repeat their reason instead of naming a fix, so the prose line prints each twice over. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: an undefined word's suggestion says what to write (define it before this use, qualify it as PKG:NAME or import its package with using), in a body and at top level, under --json-errors and --load; a census lists every kind that reaches the :1645 default, and each gets its own text or is shown unreachable; each of the four constructor-match reasons has a suggestion of its own that says what to write, and its prose reason does not repeat it; a test pins the undefined word's and each of the four, failing before. Depends: habu-a-load-s-99fed2b4 (same lines). Claim: unassigned.
