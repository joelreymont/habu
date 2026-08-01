---
title: Replace the protected-WID table with a bitmap
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-01T16:06:24.384641+02:00\""
---

Root cause of the maki competitive-evidence red (see habu-fix-maki-competitive-7dc29ec2): the protected-WID registry is a flat 256-slot u32 table (PROT-WID-MAX, src/habu/layout.f:325) that fills at one slot per public ADT family and never releases; master runs at 246/256 and the branch went over, so an innocent enum in whatever file declares next dies with uncaught 7169. Replace the flat list with a WID-indexed bitmap: membership O(1) instead of a 256-entry scan on every sealed-WID guard (PROT-WID? at src/habu/habu1.f:2651 - record publish, AOT relocation, snap-rebase are the hot callers), the cap becomes the engine's own WID bound (WIDN-CELL/WID-MAX) instead of an unrelated 256, and a 4096-WID bitmap is 512 bytes against the current 1024, fitting the existing layout gap (CC0..C0 plus the documented-movable UNCGH-CELL). Touches: src/habu/layout.f, src/habu/habu1.f (prot-wid-add, PROT-WID?), bootstrap/cg/forth.fs:121, src/habu/aot-capture.f (the capture/restore format reads the live registry at fixed offsets - the transitional-build hazard layout.f warns about), habu2.f restore bounds, the PROT-GUARD band, snapshot format. Seed-affecting: byte-fixpoint reinstall, size/census ratchets, no-binary recovery must stay green. Acceptance: maki/test.f fully green with headroom measured and reported; the synthetic 200-enum reproducer passes; all engine gates green at the fixpoint.

Claim: agent=makilane workspace=.jj-ws/habu-fix-maki-competitive-7dc29ec2
