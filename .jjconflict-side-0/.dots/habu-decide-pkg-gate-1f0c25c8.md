---
title: Decide package-gate rows for sibling mirror files
status: open
priority: 2
issue-type: task
created-at: "2026-07-30T00:25:26.313372+02:00"
---

Full context: from agent mirrorcat 2026-07-30 (commit 2cceebce). The mirror package-gate category admits exactly bootstrap/cg/forth.fs; the other 63 .fs files in the recovery corpus (29 bootstrap/cg, 27 bootstrap/src, 6 load drivers, test/nf.fs, test/bootstrap-wide-memory.fs) still report E-PACKAGE-OWNERSHIP on any definition change - measured file by file. Deliberate: the category's argument needs BOTH halves (Gforth-hosted so packaging is impossible, AND a named parity authority owns correctness) and only forth.fs has the second half today. The next agent changing bootstrap/src/checker.fs or bootstrap/cg/asm.fs hits the same wall; when that happens, name the parity authority for that file (or create it), add its row (one line at the single comparison site) plus its own measurement to tools/package-diff-lint-test.f. Do not admit the whole extension or directory wholesale. While in the file: give ENGINE-TRUNK-AT the same row bounds check MIRROR-AT has (out-of-range currently answers the last row, the admitting direction; harmless today, its sole caller is a bounded loop).
