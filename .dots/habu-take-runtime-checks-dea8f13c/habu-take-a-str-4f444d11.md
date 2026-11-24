---
title: Drop the checker's run-time length checks on a str
status: open
priority: 2
issue-type: task
created-at: "2026-11-23T19:12:11.763944+01:00"
---

Problem: the checker's words a program calls with a name or a text (CHECK!, CHECK-CANDIDATE!, CHECK:DEFINITION, PRIM-SPEC:FIND and the others test/name-length-test.f lists) already take a str, but test at run time that its length describes memory (BYTE-SPAN?) and die (CHECK-RESET, src/core/checker.f 'token buffer too large', rc 76) or answer no; test/name-length-test.f asserts it with lengths -1 and the largest cell (BODY$, BAD$). A str made by >str from a ^u8 n carries no checked bound, like every raw pointer (docs/design.md "Raw pointers stay lifetime-free"), and no other str word tests one. Root AGENTS.md: no runtime checks; instrumented checks on demand only. Acceptance: no checker word tests a str's length at run time; name-length-test keeps only its long-name cases. Depends: none. Claim: unassigned.
