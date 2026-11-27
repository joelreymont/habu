---
title: Compile a SPARQL query within 1 MiB
status: open
priority: 2
issue-type: task
created-at: "2026-11-27T11:04:36.780415+01:00"
---

Problem: Joel 2026-11-26: "your memory management is crap. we should be using 1mb max here". Queries far under Zen's 16,384-character limit map past 1 MiB (the arena's chunks double from 16 KiB): nested OPTIONAL 88 deep (2,762 bytes), 112 sibling OPTIONALs (3,076 bytes), 1,280 VALUES constants, 1,536 IN members, 3,000 || terms (4,080 KiB), a 900 KB comment, which the lexer copies whole (lex.f CODEPOINT-ESCAPES). Acceptance: every query of up to 16,384 characters compiles within 1 MiB by the arena's own count, these shapes at that size included; every SPARQL test answers as before. Depends: none.
