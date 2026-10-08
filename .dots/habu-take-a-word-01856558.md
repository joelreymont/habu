---
title: "Take a word's type from its definition"
status: open
priority: 2
issue-type: task
created-at: "2026-10-08T10:58:28.825097+02:00"
---

Problem: src/core/checker.f carries 359 PRIM:/PPRIM: lines (from :10353) that hand-write the type of words compiled before the checker runs, so the type at the definition is never recorded and a second copy stands in. Examples: DIAG-FILE! is defined at checker.f:19604 as `( ptr u8 n -- )` and re-typed at :10366; CORE-STR= (src/core/util.f:61) states its type only in a trailing comment and the row at :10361 is the only declaration.
Ruling (Joel, 2026-10-08): the definition is the source of a word's type. The kernel is the words compiled before the type checker runs; it is untyped and implicitly trusted. A kernel word that checked code calls carries a trusted type declared at its definition; every other kernel word needs no type, sits in the system package's private section and is stripped from the delivered hb. No census or certification ceremony; integration tests cover the kernel.
Acceptance: each typed word's effect is recorded at its definition and the checker reads it there; the PRIM:/PPRIM: rows for source-defined words and the PRIM:/PPRIM: definers are deleted (engine machine-code primitives keep their single row in src/habu/prims.f); kernel words checked code does not call carry no type and are absent from bin/hb.names.
Files: src/core/checker.f, src/core/util.f and the other prefix files whose words have rows, src/habu/prims.f only if needed.
Verify: full suite on the built hb.
Depends: none. Ownership: unassigned. Claim: unassigned.
