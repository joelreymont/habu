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
