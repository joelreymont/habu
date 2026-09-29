---
title: Represent x86 live-region sites as a row band
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T13:12:28.851249+03:00"
---

Problem: `callmap`/`addrmap` hold one bit per 4-byte region word (`src/habu/layout.f:1548,1604`) and `BCALLMAPSET`/`BADDRMAPSET` refuse unaligned addresses (`src/habu/habu1.f:2539,2580`); x86 sites are byte offsets (`call rel32` displacement at +1, `movabs` immediate at +2); the maps are consumed by publish, K8, `SNAP-RELOC:EMIT-CALLS` (`src/habu/habu2.f:6350`) and the capture (`src/habu/aot-capture.f:1700-1704` `ACAP-CHAIN-BIT?`).
Acceptance: `layout.f` declares, by target, either the two bitmaps (ARM64, unchanged) or a site-row band in the same bytes (x86: region byte offset u32 + kind u8, capacity refused by name); the `callmap-set`/`addrmap-set`/ `reloc-maps-clear`/`code-publish` semantics are stated per target in `prims.f` comments (on x86 the same primitives append rows, `reloc-maps-clear` clears them, `code-publish` drops them per span); `src/habu/sites.f` (package `SITES`) is the one reader, `EACH-IN-SPAN ( n n [ n n -- ] -- )` over region byte offsets and kinds, with a bitmap arm and a row arm; snapshot relocation is documented as not applicable on x86 (fixed region and DATA); `docs/x86-64.md` gains the site-row section; the ARM64 engine byte-identical (chain).
Files: `src/habu/layout.f`, new `src/habu/sites.f`, `src/habu/prims.f`, `test/sites.f`, `docs/x86-64.md`.
Verify: spark `bin/hb --load test/sites.f` drives both arms over synthetic bands; rebuild; chain gen2==gen3; gate.
Depends: none. Serialise with K2 on `layout.f`.
Route: Alder (shared: src/habu/layout.f, src/habu/sites.f, src/habu/prims.f, test/sites.f).
Ownership: krait (Intel lane).
Claim: unassigned.
