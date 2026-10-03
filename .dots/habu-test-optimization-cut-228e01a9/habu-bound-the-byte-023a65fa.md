---
title: Bound the byte reads in two more lint scanners
status: open
priority: 2
issue-type: task
created-at: "2026-10-02T10:48:54.271037+02:00"
---

Problem (r4-lintcap, 650af471): tools/public-signatures-core.f:513 and tools/repl-lint-core.f:257 use the loop guard `END? 0= CUR … and`, which reads one byte past an exactly-sized buffer because `and` does not short-circuit; the same pattern in tools/lint/source-lex.f crashed check.f with SIGSEGV rc 134 on a source whose last token ends on a 64 KiB boundary (fixed there by 650af471 with CUR-NOT?/GAP?/INK?). Acceptance: an input ending exactly at the buffer's end (64 KiB boundary) through each tool's real entry point runs without a past-end read (seen to crash or misread first); one shared bounded guard if the lexers can share it; the tools' tests rc 0. Files: tools/public-signatures-core.f, tools/repl-lint-core.f, their tests. Base: after 650af471 lands.
