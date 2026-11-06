---
title: "Lay each defer's trailer in a written program"
status: open
priority: 2
issue-type: task
created-at: "2026-11-07T00:10:18.995900+02:00"
---

Problem: a defer's host copy ends at a 16-byte trailer, DEFER-MAGIC then its dispatch cell (src/compiler/native/publish.f COMMIT-DEFER :164-168), which T0's `is W` reads through NDICT:REC-DEFER-CELL (src/habu/t0.f DO-IS :1250-1262; dict.f TRAILER-CELL :540-548). LINKER:WRITE writes each record's code from its shadow emission, which holds no trailer, and nothing in src/habu/linker.f, link-band.f, link-data.f or src/arch/arm64/link*.f writes DEFER-MAGIC (rg, 2026-11-07). Inferred, not run: an hb the cross-build writes answers E-T0-DEFER for `is` on any of its own defers. Acceptance: a written program that keeps its dictionary carries each defer record's trailer right after its code, the cell relocated as other data addresses are, so REC-DEFER-CELL answers the cell in the running product; first, a probe that shows the failure on a written hb (or shows the inference wrong and closes this). Files: src/habu/link-band.f or link-data.f (the writer's code layout). Verify: in a cross-built hb, `defer D : X ( -- ) ; ' X is D` at top level and inside a definition, then D runs X. Depends: an hb the cross-build writes that starts (S5c). Ownership: carl (lead), worker lane. Claim: unassigned. Related: design-l4.md Settled item 12 (the link lays the host copy's trailer through NPUB:IMPORT-PENDING-DEFER).
