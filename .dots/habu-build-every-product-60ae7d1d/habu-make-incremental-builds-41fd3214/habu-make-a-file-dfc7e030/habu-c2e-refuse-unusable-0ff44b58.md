---
title: "C2e: refuse unusable facts by name"
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T20:16:59.627133+02:00"
---

Problem: a record without usable facts must stop the check with a named error. Acceptance: named errors for a record written before the facts layout, a record below the build's first, an unparsable facts text, a facts text that differs from the checker's proof, and an offset in [32] outside DATA (record-facts! checks only the record index), each shown by a rejected program, among them the rows the effect text cannot say (src/core/render.f): a quotation that never returns keeps its return row, `: MKQ [: 1 >r 5 throw ;] ;` (the T-QUOT arm); a quotation whose return row holds cells (QRET-NAME); more than 26 named rows, a `defer QRS ( -- [ R -- S ] )` used 14 times (RROW-LETTER; habu-name-more-than-9146d890 makes it sayable). A record with [32] = 0 where a caller needs facts is refused only in a program the new linker wrote (after C5); before it, 0 reads as the stores. Depends: none. Claim: unassigned.
