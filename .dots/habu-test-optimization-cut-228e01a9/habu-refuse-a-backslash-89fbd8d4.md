---
title: Refuse a backslash nominal type name
status: open
priority: 3
issue-type: task
created-at: "2026-10-01T17:35:26.651793+02:00"
---

Problem: `DEFLINEAR \\` then `( a comment )` then `: CKN-USE ( n \\ -- \\ n ) swap ;` ($HOME/.cache/tmp/kestrel-r4-rev177/p/bslash-cmt.f) loads rc 0 but check.f refuses it rc 70 E-BAD-NOMINAL-TYPE with token `( a comment )`: tools/lint/source-lex.f's SOURCE loop ends the line at a token-initial \\ (LINE-COMMENT), so CHK-LIN-REGISTER (tools/check-core.f:782-783) gates whatever token starts the next line. Same class as the ( and s" shapes deftype c2 (dot 451c6cb6, de72b120) fixed. Also from review 177 (finding 2): the rationale for TYPE-BAD-BYTE? at src/core/checker.f:4575-4577 and docs/effects.md:249-251 says the refused byte opens a comment or a string, which is true only of the standalone openers; a(b, a"b, foo" are refused by the byte rule though the lexer reads them as words. Acceptance: TYPE-BAD-BYTE? refuses byte 92 and docs/effects.md names \\; check-test rows for \\ under LIN-REFUSED and REC-REFUSED fail first on de72b120's engine (bslash-cmt: loader 0 -> 70); loader and check.f agree; the rationale sentence states the actual rule (a name holding a byte that can open a comment, a string or a line comment is refused so the loader needs no copy of check.f's opener list). Rebuild, g1 = g2 with .names, two-generation build. Base: de72b120 (r4-deftype c2). Files: src/core/checker.f, docs/effects.md, tools/check-test-lib.f.
