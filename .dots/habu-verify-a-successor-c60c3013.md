---
title: Verify a successor stays in its function
status: closed
priority: 2
issue-type: task
created-at: "2026-08-13T22:57:17.180173+02:00"
---

Resolved with 10427498 on 2026-09-14: same-function successor validation and
forward/backward rejection fixtures pass on rebuilt engine SHA91ea98b7 after
independent review. This closes the duplicate, not the full compiler gate.

Model gap found by the quot-scope lane (354b8937): src/compiler/ir/verify.f never checks that a terminator's successor belongs to the same function as the branching block - a cross-function branch reached the SELECTOR in some shapes instead of being refused at freeze (it surfaced as E-IR-VERIFY-SUCCARG or -DOM only when argument counts or domination happened to disagree). Add the same-function successor check beside OPERAND-DOM-CK with a forged-module negative fixture; it turns the fc37262a class loud at the earliest gate. Files: src/compiler/ir/verify.f. Depends: none.
