---
title: "C2e: refuse unusable facts by name"
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T20:16:59.627133+02:00"
---

Problem: design r2 D9: a record without usable facts must stop the check with a named error. Acceptance: named errors for a record written before C1's layout, a record below the build's first, an unparsable facts text and D5's difference between text and proof, each shown by a rejected program, among them the three rows the effect text cannot say (C2a review, 2026-11-06): a quotation that never returns keeps its return row, `: MKQ [: 1 >r 5 throw ;] ;` (the T-QUOT arm); a quotation whose return row holds cells, the SHARE/FOO pair of /tmp/rev-c2a.41203/p1/q3.f (QRET-NAME); more than 26 named rows, a `defer QRS ( -- [ R -- S ] )` used 14 times (RROW-LETTER). A record with [32] = 0 where a caller needs facts is refused only in a program the new linker wrote (after C5); before it, 0 reads as the stores. Files: src/core/checker.f, a rejected-program test. Verify: those cases; bin/hb --load test/run.f. Depends: C2c2. Ownership: those files. Claim: unassigned.

From C2c2 r3 (2026-11-07): add a named refusal for an offset in [32] outside DATA (record-facts! checks only the record index).
