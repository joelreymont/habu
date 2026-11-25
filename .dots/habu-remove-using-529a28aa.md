---
title: Remove using
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T18:10:26.244622+02:00"
---

Problem: the user chose always prefix (2026-11-25), yet the engine keeps using (src/compiler/native/dict.f USING-REFUSED?, the checker's USING- words, E-USING-AMBIGUOUS, E-USING-SHADOW-GLOBAL) and test/using-test.f. Acceptance: using, its words and its error codes are gone, and docs/forth.md names no import. Depends: habu-call-pkg-words-9d302d82. Claim: unassigned.
