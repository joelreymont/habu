---
title: Run the lsp checks that need no engine origin
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T17:54:36.852758+01:00"
---

Problem: two lsp-boundary cases switched off by ENGINE-ORIGINS? (tools/lsp-test-lib.f) also hold checks that need no engine word's source location, which now run nowhere: definition-prefix-origin's PREFIX-STRUCT go-to-definition asks (ids 8-16, PREFIX-DEF-TURNS) and references-source-backed-builtin's prepareRename and rename refused with -32803 (REFS-BUILTIN-TURNS). Acceptance: those checks run in cases that ENGINE-ORIGINS? does not switch off, and lsp-boundary passes on a linked hb. Depends: none. Claim: unassigned.
