---
title: "Index the library's public words"
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T18:43:37.903375+02:00"
---

Problem: agents write helpers src/lib already has (test/signature-nesting.f :30-49 beside test/gate-common-lib.f's GE-SRC builder), because they find words by grepping guessed names across 215 files in src/lib, and Habu's names (STR=, SB-, BUF:, SPAN:) are not the ones they guess. src/lib/word-docs.f already reads each word's package, stack effect and comment; nothing publishes it. Acceptance: a generated docs/lib-index.md lists every public src/lib word as PACKAGE:WORD, its stack effect, path:line and its comment's first line, so a grep by concept or by effect finds it; a test fails when the index is stale; AGENTS.md says to search it before writing a helper; docs/forth-card.md gains a short table of the packages most code needs and what each is for. Depends: none. Claim: unassigned.
