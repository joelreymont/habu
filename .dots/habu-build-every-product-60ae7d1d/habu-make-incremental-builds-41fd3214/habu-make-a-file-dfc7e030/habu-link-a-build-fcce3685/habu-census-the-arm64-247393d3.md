---
title: "Census: the arm64 boot and writers for a stripped product (S6 L5)"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-05T20:17:08.535283+02:00"
closed-at: "2026-11-05T20:24:51.485431+02:00"
close-reason: "Report .out/carl-file/s6-census-l5.md: Q1-Q6 answered. Boot at offset 0 needs ASM-INIT and LABELS:INIT split out of EMIT-FORTH-IMAGE (else the writers take an entry offset); EM-MMAP-CODE-REGION needs x20 and runs before the DATA map (step 6 only); a stripped MAIN reads x19, x20, sp; the boot stores byte 0's run-time address in RBASE-CELL. Open: LUNCAUGHT/LDPBAD/LEXITHOOK reads, GENIO OUT-CELL/REPLH-CELL writers, LMMAPCODE/LBLRANGE binding, MAIN's T0 bytes; the L5 brief settles them."
---

Problem: S6 L5 (design r2 D1, D2, D3, D6, D10; .jj-ws.noindex/carl-dots/.out/carl-file/design-s6-r2.md) needs A64BOOT and the Mach-O/ELF writers for a stripped product laid out as [kernel][routines][DATA image][DATA list] in __text at a slide; the facts the brief needs (today's stripped entry and hb's startup step by step, the register and header-cell contract a minimal MAIN reaches, whether the kernel emission must start at CODE offset 0, the writer path) are not in census A-D. Acceptance: a read-only report .jj-ws.noindex/carl-dots/.out/carl-file/s6-census-l5.md with path:line evidence; no tree edit. Files: none. Verify: the report. Depends: none. Ownership: none. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-s6-l5c.
