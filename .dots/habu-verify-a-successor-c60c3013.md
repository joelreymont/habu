---
title: Verify a successor stays in its function
status: open
priority: 2
issue-type: task
created-at: "2026-08-13T22:57:17.180173+02:00"
---

Model gap found by the quot-scope lane (354b8937): src/compiler/ir/verify.f never checks that a terminator's successor belongs to the same function as the branching block - a cross-function branch reached the SELECTOR in some shapes instead of being refused at freeze (it surfaced as E-IR-VERIFY-SUCCARG or -DOM only when argument counts or domination happened to disagree). Add the same-function successor check beside OPERAND-DOM-CK with a forged-module negative fixture; it turns the fc37262a class loud at the earliest gate. Files: src/compiler/ir/verify.f. Depends: none.
