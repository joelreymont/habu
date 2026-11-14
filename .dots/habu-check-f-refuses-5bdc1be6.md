---
title: check.f refuses engine-suite.f's package definitions of char and tick
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T10:56:42.294989+02:00"
---

Problem: `hb --load tools/check.f -- test/engine-suite.f` exits 1 with E-RESERVED-DEFINITION at test/engine-suite.f:550:3 and :551:3, while the file runs green under bin/hb. Those lines define `char` and `'` inside package ES-CHAR-N (test/engine-suite.f:498-509), a deliberate case asserting that a body's `char` or `'` calls the word of that spelling; the loader admits the definitions, but check.f's reserved-name lint (tools/check-core.f:1568 CHK-RUN-RESERVED-NAMES, run at :1790) lists both as parser words no source may define (tools/reserved-name-lint-core.f:162, :168). The two layers disagree on whether a package may define these names, and which rule holds is a choice nobody has made: the lint is right and the loader should refuse them too (the case changes), or the loader is right and the lint should admit them in a package. (The dot's earlier cause, 'bad value-record field type q', no longer reproduces: an audit run of check.f on a reduced VALUE-RECORD with a q field passed.)
Acceptance: Joel's choice of the rule for defining `char` and `'`, recorded here; then the loader and check.f apply the same rule and check.f on test/engine-suite.f exits 0.
Files: tools/reserved-name-lint-core.f, tools/check-core.f, test/engine-suite.f, the loader's definition guard (src/habu/habu2.f C-RESERVED-DEF-GUARD) if the rule moves there.
Verify: hb --load tools/check.f -- test/engine-suite.f rc 0; bin/hb --load test/engine-suite.f rc 0.
Depends: none.
Ownership: unassigned. Claim: unassigned.
