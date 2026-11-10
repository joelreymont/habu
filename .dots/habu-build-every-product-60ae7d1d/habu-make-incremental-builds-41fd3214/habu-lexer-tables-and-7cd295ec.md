---
title: Lexer tables and the IR session are carried in objects
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T12:43:53.462967+02:00"
blocks:
  - habu-design-no-file-298feb05
---

Problem (census habu-census-every-cell-c247785e, by reading, kinds inferred, not reproduced): a hook registered with IMAGE-LIFECYCLE:REGISTER cleans plain cells a file object carries, so a file whose top level reaches the registration and is then linked from its object gives the program the builder's state. Cells: src/lib/source-lex.f:114 (TABLES :30 and its vectors and counters :32-62); src/compiler/ir/context.f:520 (SESSION-SLOT :418, DEPTH :55, REGION-HERE :74, REGION-EPOCH :85, SESSION-CTX :417), if that file is ever linked from an object. Files: those two. Acceptance: a reproducer as a case of test/file-objects.f where a top level can reach the registration (or the report says why none can), failing first; then no file object carries these cells' process state, by the mechanism of design habu-design-no-file-298feb05. Verify: the case; the owner's focused test; engine built twice equal when the file is engine-provided. Depends: habu-design-no-file-298feb05. Ownership: the files named. Claim: unassigned.
