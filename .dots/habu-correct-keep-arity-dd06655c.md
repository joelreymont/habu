---
title: "Correct KEEP-ARITY's comment with its reaching case"
status: open
priority: 3
issue-type: task
created-at: "2026-09-17T19:20:41.018475+03:00"
---

Problem: src/compiler/native/compiler.f:345 KEEP-ARITY says the absent SPELL-ARITY answer 'HAS NO REACHING CASE TODAY' and names a dot habu-reach-the-absent-360162f5 that no longer exists in .dots or its archive; the primitive-table lane found the reaching case 2026-09-17: a body whose branches leave different data-stack depths has no inferable effect, the checker records none, and the compiler refuses with 'ncomp: cannot compile <name>' with no ' at ' token, throw -8579 E-NCOMP-ARITY (measured on PE-SPEC-ATOM before it got its own operand stack; LESSONS.md entry under Checker Soundness). Acceptance: the comment states the reaching case and drops the dead dot reference, and a regression under test/compiler/ compiles such a body and asserts E-NCOMP-ARITY with the word named. Files: src/compiler/native/compiler.f, test/compiler/. Verify: the regression; test/run.f. Depends: none. Ownership: native compiler diagnostics. Claim: unassigned.
