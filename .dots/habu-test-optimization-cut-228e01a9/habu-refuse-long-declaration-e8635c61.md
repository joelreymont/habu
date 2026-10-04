---
title: Refuse long declaration names and deep terms by name
status: open
priority: 2
issue-type: task
created-at: "2026-10-03T04:30:17.275882+02:00"
---

Ordinary source still ends the process with die 76 at three capacities (review 389, measured on the rsbuf engine): (1) src/core/type-family.f:1377 'tfam: constructor name too long' when a SUMTYPE/STRUCTURE family tail or field name is long enough that a derived constructor or -INIT-UNPACK spelling overflows TF-CTOR-BUF (a 1100-byte tail: --load and tools/check.f both rc 76); bound tails at declaration (sumtype.f TDECL-REQUIRE-NAME / -FIELD-NAME, structure-decl.f, enum-decl.f) so the front ends refuse with their capacity code and :1377/:1381/:3023 become unreachable guards; (2) src/core/checker.f:~10307 'checker: package name too long' for a package name of 256 bytes or more; (3) src/core/checker.f:~2305 'term walk too deep' for a word with 10000 stack outputs (8191 pass). Each becomes a named, located refusal through the existing declaration or checker diagnostics, tested through tools/check.f; docs/forth.md 'Engine limits ordinary source reaches' and the card list the ceilings.
