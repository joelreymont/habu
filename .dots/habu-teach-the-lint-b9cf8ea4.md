---
title: Teach the lint tokenizer about string literals
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T18:54:49.896685+03:00"
---

Problem: tools/lint/token.f splits source on whitespace with no model of a string literal, so the payload of an escaped string such as `s\" \n"` reaches the lints as a chunk starting with a backslash, which is Forth's line comment; the rest of that line, including a definition's `;`, vanishes and the clobber lint then analyses two definitions as one (profiler lane, 2026-09-16: a false CLOBBER in src/habu/prof.f c-prof-rep-row, bisected to `s\" \n"` versus `s" x"`). The gap can also HIDE a real clobber anywhere in the scanned files. The profiler worked around it by starting every escaped payload with an ordinary byte. Acceptance: the shared lexer skips `s"`, `s\"`, `c"`, `."`, `.(` and `\` comments correctly (the tree already has tools/lint/source-lex.f: use one lexer for both), with a fixture per literal form; clobber-lint's MIN-ROUTINES and call census tripwires re-baselined with the new counts stated in the commit; the prof.f workaround reverted to the natural spelling in the same commit; every lint suite green. Files: tools/lint/token.f, tools/lint/source-lex.f, tools/lint/clobber-lint.f, tools/lint/*-test.f, src/habu/prof.f. Verify: tools/lint/clobber-lint-test.f; tools/lint/clobber-lint.f; test/run.f. Depends: none. Ownership: lints. Claim: unassigned.
