---
title: "A colon packet declares the body's rows for a signature with no record"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T14:16:42.366423+02:00"
---

Problem: src/core/render.f DIAG-JSON writes declared_effect and declared_effect_source whenever a signature was seen (SGSEEN), and DECL-EFFECT falls back to the live rows when SIG-EFF-CACHE! built no record (a malformed signature, SGBAD, or one too wide or deep, UNFIT): ': GZ ( ) + ;' gives declared_effect "n n -- n n " and source "", and a 256-input signature gives 64 n. Found by the review of habu-a-does-clause-d74786de (finding 2-1, same cause for a does> clause); cases /Users/joel/Work/reviews/habu-10x-2026-11-10/cases8/recdoes/c2. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: a packet writes both fields only from the record, null for both without one; DECL-EFFECT has no fallback; a case for a colon and one for a clause fail before. Files: src/core/render.f, src/core/checker.f, test/compiler/input-underflow-refusals.f. Verify: the cases; the full suite. Depends: none; folded into the lane of habu-a-does-clause-d74786de. Ownership: the checker's diagnostics. Claim: agent=bob workspace=.jj-ws.noindex/habu-a-does-clause-d74786de
