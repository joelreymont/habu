---
title: docs/forth.md names files, labels and words that do not exist
status: open
priority: 2
issue-type: task
created-at: "2026-08-22T22:38:26.024916+02:00"
---

Problem: docs/forth.md:929-930 and :952 'src/config.fs' (missing; the same file says codes live in lib/errors.f at :228-239); :77 'test/gate-dictionary-lib.f GD-LITERAL-FIRST' and :93 'GD-LITERAL-FLOAT-FIRST' (the first only in comments, the second nowhere); :622 'around 71 STRUCTURE and 207 ENUM sites' (measured 81/237; type-families.md:795 repeats it); :1105 USES/USED-BY are not words (docs/debugging.md:108 has the real surface); :909 '@EXECUTE' 0 occurrences; :130-540 is a 410-line exceptions ledger restating what tools/package-diff-lint enforces. Acceptance: the five references corrected; the ledger replaced by the rule plus a pointer at the lint fixture. Files: docs/forth.md, docs/type-families.md. Verify: a script checks every path and backticked word in forth.md against the tree. Depends: none. Ownership: docs. Claim: unassigned.
