---
title: Let the JSON reader grow its nesting stack
status: open
priority: 1
issue-type: task
created-at: "2026-10-27T20:25:37.205068+02:00"
---

Joel 2026-10-27: no JSON nesting cap; callers pass only the document and never size storage or depth. Design (Fable Plan, adopted): .jj-ws.noindex/carl-dots/.out/carl-file/design-jr-cursor.md: opaque JR:cursor record, 1 bit per level, 64 inline, one doubling mapping past it; JR:INIT ( ptr JR:cursor ptr u8 n -- JR:reader ); new JR:DISPOSE; STORAGE-BYTES, E-JR-DEPTH, E-CAPACITY, E-STORAGE retire. Lanes: A core (carl-jra), then B src/lib consumers, C LSP tools, D other tools, E test entries and docs. Landing: full suite, install, then Zen/Tender rewrite.

Lane A 2026-10-27: .jj-ws.noindex/carl-jra (second worker after a hand-off; root-causing a token-order bug in the rewrite).
