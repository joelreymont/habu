---
title: "Lay each defer's trailer in a written program"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-07T00:10:18.995900+02:00"
closed-at: "2026-11-10T21:50:20.573522+02:00"
close-reason: "done: src/habu/link-band.f:466-477 writes DEFER-MAGIC and the dispatch cell; `is` inside two definitions retargets a defer on master's engine (prints 7 then 8); a top-level `is` is not Habu"
---

Problem: a defer's host copy ends at a 16-byte trailer, DEFER-MAGIC then its dispatch cell (src/compiler/native/publish.f COMMIT-DEFER :164-168), which T0's `is W` reads through NDICT:REC-DEFER-CELL (src/habu/t0.f DO-IS :1250-1262; dict.f TRAILER-CELL :540-548). LINKER:WRITE writes each record's code from its shadow emission, which holds no trailer, and nothing in src/habu/linker.f, link-band.f, link-data.f or src/arch/arm64/link*.f writes DEFER-MAGIC (rg, 2026-11-07). Inferred, not run: an hb the cross-build writes answers E-T0-DEFER for `is` on any of its own defers. Acceptance: a written program that keeps its dictionary carries each defer record's trailer right after its code, the cell relocated as other data addresses are, so REC-DEFER-CELL answers the cell in the running product; first, a probe that shows the failure on a written hb (or shows the inference wrong and closes this). Files: src/habu/link-band.f or link-data.f (the writer's code layout). Verify: in a cross-built hb, `defer D : X ( -- ) ; ' X is D` at top level and inside a definition, then D runs X. Depends: an hb the cross-build writes that starts (S5c). Ownership: carl (lead), worker lane. Claim: unassigned. Related: design-l4.md Settled item 12 (the link lays the host copy's trailer through NPUB:IMPORT-PENDING-DEFER).

Confirmed (carl, 2026-11-08, from the maps): bin/hb.map gives TFAM-RESOLVE-XT 44 bytes with the next word 60 bytes on (code plus the 16-byte trailer); a linker-written progG.map gave it 40 bytes with the next word right after.

Fix (carl-c5a2 kqunxzrx, src/habu/link-band.f only): LAYOUT leaves TRAILER-BYTES after each placed emission whose record NDICT:TRAILER-CELL names a dispatch cell (EM-TRAILER, which dies when the defer does not end its emission); PLACE-ONE writes DEFER-MAGIC, then the cell through the link's data relocation. The map shows the trailer as a gap row, as link-map.f's header says of bytes no record spans.

Proof (carl-a2w, /tmp/carl-a2wd/hb, census-Gt = census-G with .out/a2w/engine/engine-entry-t.f): the written program reads D's trailer as TRAILER-CELL does, stores HELLO's entry in the cell and runs D, printing `defer: engine: hello`; cold compiled 67 (3.7 s), warm 0 (2.0 s), program, map and objects byte-equal cold to warm. test/file-objects.f passes. Still open: the acceptance's T0 `is` in a cross-built hb, which waits on S5c.

Status (carl, 2026-11-08, /tmp/carl-hb.aM/hb, census-M progM): progM runs `2 3 * . cr` (6), `--load t1.f` (3), and FOO (E-UNDEFINED, rc 70) as hb does, but any colon definition still crashes it (`: SQ dup * ; 3 SQ . cr`, SIGSEGV rc 139; jerry's item A, TFAM-FIND-IN), so the acceptance's `is` inside a definition waits on that.
