---
title: Move remaining lints onto shared lexer
status: open
priority: 2
issue-type: task
created-at: "2026-07-28T22:56:11.564778+02:00"
---

Full context: TOKENIZE plus per-tool comment and string guessing still backs tools/maki-dep-lint-core.f, tools/repl-lint-core.f, tools/namespace-lint-core.f, tools/lint/clobber-lint.f, and the PRIM scan in tools/lint/shadow-lint.f. Each carries the same blindness class just closed for error codes: TOKENIZE strips from a backslash to end of line with no string awareness, so a backslash inside a string body swallows that string's closing quote and inverts any in-string tracking for the rest of the file. The error-code lint proved this is not theoretical — it silently skipped real claims on the live tree. Audit each consumer, port it to LINT-LEX, add the backslash-in-string and bare-quote negative fixtures per tool, and retire TOKENIZE when its last caller is gone. Acceptance: each ported tool has a fixture that reds if the heuristic is reinstated.
