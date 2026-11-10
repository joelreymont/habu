---
title: Tier 0 refuses a missing runtime word without naming it
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T12:47:37.164113+02:00"
---

Problem: src/habu/t0.f TAG-INLINE (:1182) and the NFETCH-CHECK:TAGS site (:1204) throw E-HIR-UNMODELED when NDICT:CALL-TARGET finds no entry; REFUSED$ (:2031) answers an empty token for that code, so the line reads 'ncomp: cannot compile JW-BUF@' with no word or reason. Found by the json.f build hunt. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: the refusal names the runtime word the product lacks. Files: src/habu/t0.f. Verify: a build of a source that fetches an enum with the word absent prints the word's name. Depends: none. Ownership: tier 0. Claim: unassigned.

Waits 2026-11-10 (bob): habu-a-built-program-e5617780 makes fetch-check.f a boot row, so a product may never lack the word; that lane's review answers whether a reproducer remains (brief /Users/joel/Work/reviews/habu-10x-2026-11-10/briefs7/Review-boot-row-for-fetch-check.md, claim 6). Brief this dot from its answer.

Reproducer 2026-11-10 (review of habu-a-built-program-e5617780, claim 6): the JW-BUF@ path is gone, since every build now carries the package; what remains is a package part named NFETCH-CHECK that lacks BAD-TAG (/Users/joel/Work/reviews/habu-10x-2026-11-10/cases8/boot/src-probes/part-missing.f: "ncomp: cannot compile L-GET", threw -8286, rc 74; part-missing-control.f builds). habu-a-pkg-part-3ae2acf8 binds the words at the top level, which probably removes this path: recheck after it lands, and close this dot if no reproducer is left.
