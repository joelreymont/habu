---
title: Teach verify-source the print-paren
status: open
priority: 2
issue-type: task
created-at: "2026-07-28T22:56:11.553598+02:00"
---

Full context: tools/lint/source-lex.f now treats .( ... ) as an inert printing comment, matching the native engine and bootstrap/src/parsing.fs DO-PAREN. src/habu/verify-source.f NEXT (around line 155) still skips only backslash line comments, a standalone open paren, and the two string-opener classes, so the checker's own source replay and the shared lint lexer now disagree on any source containing .( — two source readers giving two answers about what is code. Add .( handling to NEXT and a differential fixture that runs the same text through both readers and asserts identical token streams. This is the same obligation the char / [char] divergence note in source-lex.f already records for dot habu-consume-registry-events-efe7fe5e.
