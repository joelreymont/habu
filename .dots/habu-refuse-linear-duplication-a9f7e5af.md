---
title: Refuse linear duplication inside a quotation output
status: open
priority: 2
issue-type: task
created-at: "2026-10-29T21:26:44.801443+02:00"
---

Problem: `: P1D ( -- [ QLIN:tok -- QLIN:tok QLIN:tok ] ) [: dup ;] ;` defines on every engine, as do `[: drop ;]` and `[: 8 MEM-ALLOC-PTR ! ;]` typed [ tok -- ]: a quotation duplicates, drops or stores a linear token that a colon body with the same code is refused for. Cause: the declared output (CHECK-FRESH-OUTPUT, src/core/checker.f about :25896) binds the quotation variables after the last LIN-TAINT-SCAN has run, so the scan never sees the token type. Found by the fetch R1 review (tim); probes in .jj-ws.noindex/tim-r1/.out/review-r1/. Acceptance: the linear scan judges the quotation effect after the declared output binds its variables (or the binding happens before the last scan), so the three shapes are refused with the diagnostic the colon body gets, and a quotation that threads the token ( [ tok -- tok ] ) still certifies; refused fixtures for dup, drop and store plus a certifying control; the docs/forth.md linear section if it pins quotations. Files: src/core/checker.f, linear fixtures under test/compiler/. Verify: the fixtures; three generations with cmp; test/run.f. Depends: none. Ownership: checker. Claim: unassigned.
