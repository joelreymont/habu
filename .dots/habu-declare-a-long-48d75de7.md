---
title: Declare a long pointer chain of families
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T12:57:24.520294+01:00"
---

Problem: 3000 families each holding a pointer to the previous, then TYPED-VARIABLE V s3000 (/tmp/carl-chain-repro/u1.f), exit 76 'checker: term walk too deep (cyclic term)'; k=2730 passes, 2731 dies. Cause: STORAGE-VALIDATE -> STORAGE-TERM-OK? -> STORED-TERM-HIT (src/core/checker.f) runs CAST-MAY-LINEAR* and two CAST-INTRO* walks, which go under pointers and build each family's members (TFAM-MEMBERS-XT) as they reach it: three TWALK-DEEPER levels per family against TWALK-MAX-DEPTH $2000 (8192). Family-level linearity (TFAM-CONCRETE-LINEAR?, TFM-LINEAR in src/core/type-family.f) stops at pointers and is cached per family; these walks are not. Acceptance: the chain at 3000 and 20000 families declares and is used, or is rejected at its place with the limit and a fix; a test at the old failing count. Depends: none. Claim: unassigned.
