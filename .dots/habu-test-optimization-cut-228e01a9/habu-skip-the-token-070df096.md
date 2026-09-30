---
title: Skip the token a top-level parsing word takes in the pre-pass
status: active
priority: 2
issue-type: task
created-at: "\"2026-09-30T17:19:48.790492+02:00\""
---

Problem: the source pre-pass top-level loop (src/habu/verify-source.f VERIFY-SOURCE) reads every token as if it were interpreted, so a token that a parsing word consumes is taken for a colon, a definer or a buffer count, and tools/check.f refuses a source the real load path accepts. Measured 2026-09-30 on the r4-check engine (change pzrymmlt), each loading rc 0: `char : constant COLON` checks rc 1 (E-RESERVED-DEFINITION, `constant` read as a definition name); `' TYPED-BUFFER constant TB` checks rc 67 (7121 from preverify); `char 0 TYPED-BUFFER B n` checks rc 67 (7121, the 0 read as the count while the engine sizes 48 rows). A top-level string is already skipped (`s" a : b TYPED-BUFFER" type` checks rc 0). Inside a definition PARSE-NEXT? already skips the token after `char` and `[char]`. No source in the tree has the shape today. Acceptance: at top level the pre-pass skips the token each engine parsing word consumes, by one rule shared with the body scan where the words are the same; the three inputs check rc 0 and a real bad count or undefined name after such a line is still refused; cases run through the real check.f load path (tools/check-test-lib.f), written before the code; docs/forth.md states the rule the code follows. Files: src/habu/verify-source.f, tools/check-test-lib.f, docs/forth.md. Verify: tools/check-test.f rc 0. Depends: habu-make-check-f-ab22f852. Ownership: those files. Claim: agent=kestrel workspace=.jj-ws/r4-check.
