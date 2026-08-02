---
title: Lex error-code claims with the shared lexer
status: open
priority: 2
issue-type: task
created-at: "2026-07-25T14:41:35.080243+02:00"
---

Why this is needed: the error-code lint decides whether a token sits inside a string by counting quote characters, not by lexing the source. In tools/error-code-lint-core.f, ECL-STEP (line 232) toggles a single in-string flag whenever a token contains an odd number of double quotes (ECL-QUOTES-ODD?). That is a value heuristic standing in for a structural fact, and it fails in both directions.

Measured on master 79c50e5a9dbf by calling the lint's own ECL-COUNT entry point:
- `[char] " drop  -9001 constant E-XA  -9001 constant E-XB` reports 0 findings. One bare quote character flips the flag on, and every remaining claim in the file is silently skipped, so a real duplicate throw code is missed. The gate goes blind for the rest of that source.
- The same two claims without the quote report 1 finding, which is the correct verdict.
- `.( -9001 constant E-XA )  -9001 constant E-XB` reports 1 finding. The body of a print-paren is not code, so the lint invents a claim for E-XA that does not exist and reports a collision that is not there.
The existing string case in tools/error-code-lint-test.f (MECLT-NO-FALSE-POSITIVE) passes by accident: it uses a closing quote glued to the name, which changes the name token instead of exercising the string rule.

No tracked source under the scanned roots currently contains a bare quote token, so the live ledger is not blind today. This is latent breakage in a gate that guards a global namespace, not an active failure, which is why it is priority 2.

Owned result: tools/error-code-lint-core.f gets its tokens from the shared comment-and-string-aware lexer in tools/lint/source-lex.f, which already classifies line comments, paren comments, and both the plain and escaped string openers, instead of TOKENIZE from tools/lint/token.f plus the quote-parity flag. Delete ECL-QUOTES-ODD? and the ECL-INSTR flag rather than patching their arithmetic. Keep every deliberate allowance the header documents unchanged: negative codes only, FIRST/LAST range sentinels and their reserved ranges, identical code-and-name re-registration, and the bootstrap exclusion.

Acceptance and smallest owning check: the three measured cases above become fixtures in tools/error-code-lint-test.f and give 1, 1 and 0 findings respectively. Add hostile cases for an escaped string opener, a quote inside a paren comment, a claim inside a line comment, and a claim split across a string boundary. Every existing case in that file keeps its current expected count, and MECLT-LIVE still certifies the real tree clean. A mutation that reinstates the quote-parity toggle must red the new false-negative fixture.

Verify: bin/hb --load tools/error-code-lint-test.f, the gate entry point ERROR-CODE-LINT-STRICT on the real tree, and typed-local-diff-lint and package-diff-lint on the exact diff.

Files: tools/error-code-lint-core.f and tools/error-code-lint-test.f. Claim: unassigned.
