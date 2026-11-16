---
title: Count tokens in the x86-64 tokenizer
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T13:46:13.062300+01:00"
---

Problem: the arm64 tokenizers bump TOKENS-CELL (src/habu/layout.f) for each token read (interpreter.f TOKEN, habu1.f TOK-GOT), and src/core/checker.f SRCLOC HELD? holds a literal's record while the count is the recorded one or one more; the x86-64 tokenizer (src/habu/kernel-x64.f, its TKA-CELL stores) does not bump it, so on an x86-64 engine HELD? holds for every token while the buffer is unchanged and a loader's record can be placed at a stale literal. Acceptance: on an x86-64 engine, test/diag-position-test.f TEST-LITERAL-SPENT passes. Depends: x86-64's turn (x86 is off). Claim: unassigned.
