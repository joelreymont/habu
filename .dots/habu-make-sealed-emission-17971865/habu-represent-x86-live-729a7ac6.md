---
title: Represent x86 live-region sites as a row band
status: closed
priority: 2
issue-type: task
created-at: "2026-09-29T13:12:28.851249+03:00"
closed-at: "2026-09-29T16:00:06.947154+03:00"
close-reason: landed on master c595e06e (Alder); interdiff against the reviewed bookmark empty
---

Problem: `callmap`/`addrmap` hold one bit per 4-byte region word (`src/habu/layout.f:1548,1604`) and `BCALLMAPSET`/`BADDRMAPSET` refuse unaligned addresses (`src/habu/habu1.f:2539,2580`); x86 sites are byte offsets (`call rel32` displacement at +1, `movabs` immediate at +2); the maps are consumed by publish, K8, `SNAP-RELOC:EMIT-CALLS` (`src/habu/habu2.f:6350`) and the capture (`src/habu/aot-capture.f:1700-1704` `ACAP-CHAIN-BIT?`).
Acceptance: `layout.f` declares, by target, either the two bitmaps (ARM64, unchanged) or a site-row band in the same bytes (x86: region byte offset u32 + kind u8, capacity refused by name); the `callmap-set`/`addrmap-set`/ `reloc-maps-clear`/`code-publish` semantics are stated per target in `prims.f` comments (on x86 the same primitives append rows, `reloc-maps-clear` clears them, `code-publish` drops them per span); `src/habu/sites.f` (package `SITES`) is the one reader, `EACH-IN-SPAN ( n n [ n n -- ] -- )` over region byte offsets and kinds, with a bitmap arm and a row arm; snapshot relocation is documented as not applicable on x86 (fixed region and DATA); `docs/x86-64.md` gains the site-row section; the ARM64 engine byte-identical (chain).
Files: `src/habu/layout.f`, new `src/habu/sites.f`, `src/habu/prims.f`, `test/sites.f`, `docs/x86-64.md`.
Verify: spark `bin/hb --load test/sites.f` drives both arms over synthetic bands; rebuild; chain gen2==gen3; gate.
Depends: none. Serialise with K2 on `layout.f`.
Route: Alder (shared: src/habu/layout.f, src/habu/sites.f, src/habu/prims.f, test/sites.f).
Ownership: krait (Intel lane).
Claim: agent=krait workspace=.jj-ws/habu-represent-x86-live-729a7ac6.
Preflight corrections (these override the lines above where they differ):
- Mechanism: `src/` has no `[if]`, `layout.f` has no `HB-TARGET` reference, and a top-level `if` is `E-UNDEFINED` on the product engine. `layout.f` therefore declares both views of the same bytes unconditionally: the two bitmaps as today, plus, in `SNAP-RELOC`, the row band over `CALLMAP-OFF..ADDRMAP-END` (2 MiB): `SITE-N-CELL` (a u64 count at `CALLMAP-OFF`), `SITE-ROWS-OFF`, `SITE-ROW-BYTES 5` (offset u32 LE + kind u8), `SITE-CAP`, the kinds `SITE-CALL`/`SITE-ADDR`, and the exit status `SITE-RC` 103 (`src/core/engine-error.f:24` ends at 102). The arm is chosen at run time by `HB-TARGET-LINUX-X86-64?`; K2 uses the same mechanism.
- Offsets: a site's offset is the region byte offset of its instruction's first byte on both arms (the BL word or the address chain's first MOVZ; the `call` opcode or the `mov r64, imm64` REX byte, with the patched field at +1 or +`MOVABS-IMM-OFF`). `callmap-set`/`addrmap-set` take that address on x86. `code-publish` drops rows of both kinds in [dst, dst+len).
- Reader: `SITES:EACH-IN-SPAN ( off len [ off kind -- ] -- )` yields in ascending offset order on both arms. The arms are public and take their band(s) as `ptr u8` (`BITMAP-EACH` over the two bitmaps, `ROWS-EACH` over count + rows), so `test/sites.f` drives both over scratch buffers on spark. A count above `SITE-CAP`, or an offset at or after `REGION`, is refused. The ARM64 call arm yields region-to-text calls only (`habu2.f:659-668`, `5565-5567`); nothing records region-to-region calls.
- Scope: this leaf ADDS the reader. `aot-capture.f` (X2a), `aot-closure.f`/`address-carrier.f` (I7) and `publish.f` (P3) migrate later; the `habu1.f`/`habu2.f` engine readers and writers are untouched; `sites.f` is required only by `test/sites.f` here. "The ARM64 engine byte-identical" means the chain's gen2 == gen3: `layout.f` is in the product, so its bytes differ from master's engine.
- Files add: `test/gate-stdlib-cases.f` (a `SUITE sites` row like the one at `:2035`).
