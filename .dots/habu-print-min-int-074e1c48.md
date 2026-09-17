---
title: Print MIN-INT correctly
status: open
priority: 2
issue-type: task
created-at: "2026-09-17T20:02:48.714987+03:00"
---

Problem: '.' misprints MIN-INT as garbage (measured 2026-09-17 by the parity lane: MIN-INT . prints -'..--).0-*(+,))+(0( while 1 63 lshift 1 - prints 9223372036854775807 correctly, so the printer, not lshift, is wrong): the number printer negates MIN-INT, which wraps to MIN-INT, and then formats a negative value's digits. Acceptance: the printer handles the one value whose negation overflows (divide as unsigned or peel one digit before negating), a regression printing MIN-INT and MIN-INT+1 through '.', 'u.' and the fmt library, the same fixed in any second printer (lib/fmt.f, the REPL's number output). Files: src/habu/ (the printer), lib/fmt.f, test/. Verify: the regression; test/run.f. Depends: none. Ownership: engine output. Claim: unassigned.
