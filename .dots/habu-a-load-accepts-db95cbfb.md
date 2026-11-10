---
title: A load accepts a definition into a sealed package and an undefine of its word
status: open
priority: 1
issue-type: task
created-at: "2026-11-10T14:26:10.063968+02:00"
---

Problem: under bin/hb --load, after seal-package, ': SP:B ( -- ) ;' and 'undefine SP:A  : SP:A ... ;' are accepted (/Users/joel/Work/reviews/habu-10x-2026-11-10/cases8/join-review/p2/seal2.f prints 'new A', rc 0), and ': ENGINE-INTERNAL:FOO ( -- ) ;' is accepted (sealq.f); tools/check.f refuses ': SP:B' with E-SEALED-PACKAGE. docs/forth-card.md:59-62 and :168 say a definition into a sealed package is E-SEALED-PACKAGE, rc 84. Consequence: NFETCH-CHECK (src/compiler/native/fetch-check.f) is not sealed, and h1.f (reopen it, undefine BAD-TAG, define one that returns) and h2.f (undefine NFETCH-CHECK:BAD-TAG, then a qualified definition) switch off tier 0's tag trap, built and under --load ('passed', rc 0, results in p2h.txt); sealing NFETCH-CHECK alone would not stop h2. Found by the review of habu-a-pkg-part-3ae2acf8 (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/join-tagcheck-review.md, commit 2 finding 1). Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: a hunt names as path:line why the load path accepts a qualified definition into a sealed package, and an undefine of a sealed package's word, where tools/check.f refuses; then seal2.f and sealq.f exit 84 with E-SEALED-PACKAGE under --load and in a build, NFETCH-CHECK is sealed, h1.f and h2.f are refused, and a test pins each. Files: src/habu/outer.f, src/habu/habu2.f, src/compiler/native/fetch-check.f, test/seal.f. Verify: seal2.f, sealq.f, sealr.f, sealu.f, h1.f, h2.f on bin/hb. Depends: habu-a-pkg-part-3ae2acf8 landed. Ownership: package sealing. Claim: unassigned.
