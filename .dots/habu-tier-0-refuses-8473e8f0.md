---
title: Tier 0 refuses a missing runtime word without naming it
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T12:47:37.164113+02:00"
---

Problem: src/habu/t0.f TAG-INLINE (:1182) and the NFETCH-CHECK:TAGS site (:1204) throw E-HIR-UNMODELED when NDICT:CALL-TARGET finds no entry; REFUSED$ (:2031) answers an empty token for that code, so the line reads 'ncomp: cannot compile JW-BUF@' with no word or reason. Found by the json.f build hunt. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: the refusal names the runtime word the product lacks. Files: src/habu/t0.f. Verify: a build of a source that fetches an enum with the word absent prints the word's name. Depends: none. Ownership: tier 0. Claim: unassigned.
