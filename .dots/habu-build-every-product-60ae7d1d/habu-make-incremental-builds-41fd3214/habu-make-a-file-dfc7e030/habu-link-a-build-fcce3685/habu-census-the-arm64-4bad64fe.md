---
title: "Census the arm64 kernel's reach for a fixed region"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-05T15:31:07.983689+02:00"
closed-at: "2026-11-05T15:40:37.807915+02:00"
close-reason: census D written
---

S6 census D: which references from the arm64 primitive bodies and their helpers leave that code (to J/B/D labels, the payload, x20/RBASE-CELL), so the kernel can be copied to REGION-VA and __text keep only boot-arm64.f. Brief .jj-ws.noindex/carl-dots/.out/carl-file/brief-s6-census-d.md. Workspace .jj-ws.noindex/carl-s6d (read-only). Ownership: carl. Claim: carl.

Done: report .jj-ws.noindex/carl-dots/.out/carl-file/s6-census-d.md. The primitives are not closed: they branch to habu2.f die, crash, source-exec and REPL-route code (LDIE, LDPBAD, LSOURCEEXECFINISH, LREPLROUTE, LEXITHOOK), reach the interpreter through boot-filled DATA cells (LMAIN, LEVALREC, LCREATE, LRREC), and adr past the code end (LKERNELDIGEST). RBASE-CELL readers survive a move.
