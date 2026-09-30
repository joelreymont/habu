---
title: Stop top-level char from filling the body buffer
status: open
priority: 3
issue-type: task
created-at: "2026-09-30T12:45:25.145193+03:00"
---

Problem: at top level the ARM64 engine's `char` also appends its operand to the definition-body text buffer. A file of 3000 lines `char ABCDEFGH drop` dies at line 889: `hb: definition body text full at 8000 bytes: ABCDEFGH needs 8001`, rc 71. Found by I4b (habu-interpret-literal-keywords-0fa50d62), whose Habu loop does not append.
Acceptance: interpreting `char` outside a definition leaves the body buffer unchanged; the 3000-line file runs to completion with rc 0; inside a definition the recorded body text is unchanged.
Files: `src/habu/habu2.f` (the `char` keyword body, near 4542-4550), a gate case.
Verify: spark: the reproducer through `--load`; rebuild, five-generation chain, gate.
Ownership: krait (Intel lane).
Claim: krait.

Preflight corrections (2026-09-30; override the lines above where they differ):
- Acceptance: C-CHAR (`src/habu/habu2.f:4557-4565`) no longer appends its operand to the body capture: drop `LBCAP LABEL@ BL,` at 4563, as the Gforth mirror (`bootstrap/cg/forth.fs:4551-4557`) already does. A file of 3000 `char ABCDEFGH drop` lines loads with rc 0 (today rc 71 at line 889 on `hb-master-3dc7`). `char` inside a definition is refused today (`E-UNDEFINED: char`, rc 70), so "inside a definition the body text is unchanged" is replaced by these kept behaviours: `[char]` keeps its operand capture (C-BCHAR 4567-4574 untouched; `src/core/type-family.f:1254` compiles `[char] T` in a checked body); the `TOP-EV-CHAR` event (`test/top-row-hook-test.f:166`); the pushed value (`test/outer-interpret.f` TICK-AND-CHAR, 312-325). Nothing reads the body buffer after a top-level `char` (every definer resets `BODYLEN` before seeding the name: `habu2.f` 3718, 3765, 3828, 3937, 4133, 7841).
- Files: `src/habu/habu2.f` (C-CHAR); `src/habu/outer.f:685-689` (reword the comment to what the loop does, with no claim about the engine); `test/outer-interpret.f`: one case in package `OUTER-INTERPRET-TEST`, run through `BOTH`, whose top-level `char` operands go past `BODYBUF-CAP`: before the fix it fails (engine rc 71, Habu loop rc 0), after it both routes end rc 0 with the same output.
- Verify: spark, the case through both routes; rebuild, five-generation chain, gate.
