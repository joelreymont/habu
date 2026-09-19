---
title: Teach the lint tokenizer about string literals
status: active
priority: 2
issue-type: task
created-at: "2026-09-16T18:54:49.896685+03:00"
---

Claim: alder, .jj-ws/alder-lint-literals from d3c99b9b. Hazel released
tools/lint and its tests plus only the C-PROF-BYTE workaround block in
src/habu/prof.f. Prove shared literal lexing before changing that block;
report three-generation engine identity or stop with the word disassembly.

Ready: TOKENIZE projects LINT-LEX events; literal payloads are one opaque
token, including the closing quote required by primitive-name and REPL-path
consumers. Removed the lexer's unused dependency on token.f/lib.f. Malformed
input refuses before publishing tokens. Fixtures cover all normal/escaped
string openers, escaped quotes, printing/line comments, definition boundaries,
and a real clobber after inert emitter-shaped literal text. The old scanner
fails the new boundary fixture; the projection passes. C-PROF-NL/DQ use natural
literals after that proof; C-PROF-BYTE remains for the decimal-point caller.

Private /tmp/alder-lint-literals: all three engine generations are byte-identical,
SHA256 8708d227f51a56ad920b7d4b3c159dde31f9f598a1b2089df8d0175137ffad79.
All 20 affected lint/reader registry rows pass, including complete hb-build,
tail-pure and tool-boundary groups. Gen3 passes profiler-index and gate-debug
(the text/JSON profiler checks included). Astra review is clear.
The d3c99b9b production census is 399 routines/625 calls both before and after;
raise the stale call floor from 623 to 625. Hazel's later BWAITRC retirement
removes two routines, so preserve its 397 routine floor when duplicating.
No full gate here; Hazel's integration chain owns it.

Problem: tools/lint/token.f splits source on whitespace with no model of a string literal, so the payload of an escaped string such as `s\" \n"` reaches the lints as a chunk starting with a backslash, which is Forth's line comment; the rest of that line, including a definition's `;`, vanishes and the clobber lint then analyses two definitions as one (profiler lane, 2026-09-16: a false CLOBBER in src/habu/prof.f c-prof-rep-row, bisected to `s\" \n"` versus `s" x"`). The gap can also HIDE a real clobber anywhere in the scanned files. The profiler worked around it by starting every escaped payload with an ordinary byte. Acceptance: the shared lexer skips `s"`, `s\"`, `c"`, `."`, `.(` and `\` comments correctly (the tree already has tools/lint/source-lex.f: use one lexer for both), with a fixture per literal form; clobber-lint's MIN-ROUTINES and call census tripwires re-baselined with the new counts stated in the commit; the prof.f workaround reverted to the natural spelling in the same commit; every lint suite green. Files: tools/lint/token.f, tools/lint/source-lex.f, tools/lint/clobber-lint.f, tools/lint/*-test.f, src/habu/prof.f. Verify: tools/lint/clobber-lint-test.f; tools/lint/clobber-lint.f; test/run.f. Depends: none. Ownership: lints. Claim: unassigned.
