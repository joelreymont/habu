---
title: "Ship checker data only for the surface hb offers"
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T16:49:49.724818+03:00"
---

Problem: `hb` carries checker data for thousands of words no user source can name. Checking happens only when user source is compiled against a word; engine words calling each other are compiled machine code and are never checked again. Joel (2026-09-30): "don't we ONLY need type checker data for public words since there should be no typechecking during calls within the engine; same for signatures." So signatures, symbols, effects and the rest of the checker data belong in the image only for the words user source may name, plus what their signatures reference; and code, records and names that nothing can reach do not belong in it either.

Measured on master's engine (2,493,559 bytes, macOS arm64; `tools/engine-size.f`, `tools/data-table-census.f`):
- `aot/code-blob` 1,390,080 B (55.7%); `aot/data-cell-values` 640,716 B and `aot/data-cell-bitmap` 48,568 B (27.5%); dictionary records 59,932 B, name pool 65,648 B, code spans 59,856 B.
- The DATA census charges the `DONE` row, which holds the baked user-signature store and the tables of packages loaded after the REPL, 439,162 B, and the checker's symbol text `SYM-STR-BOOT` 184,437 B.
- From the engine-entry roots, 3,757 records are unreachable: 110,536 B of code, 35,738 B of records and 38,425 B of names.
- 3,250 global and 2,820 package-public records ship; private records are already dropped at capture (14 remain).
- For comparison, every instruction-selection pattern the ARM64 code-size campaign counted summed to a ceiling of 165 KB (`docs/engine-size.md`).

Acceptance: the image carries checker data only for the declared surface and what its signatures reference, by one structural rule; a user program that names a word outside the surface is refused by name at check time; public words are checked exactly as before; the byte fixpoint, the full gate and the downstream builds (Tender, Etch, Loom) pass; `tools/engine-size.f` before and after, recorded in `docs/engine-size.md`.
Children, in order:
1. habu-give-every-baked-9ca94f18 and habu-drop-private-signatures-974304d0, in parallel. Closed on evidence: habu-zero-the-type-96a51b7e (those planes never reach the image) and habu-certify-engine-src-0746d42c (certification replays its own private words, so it stays on the build host).
2. habu-seal-every-captured-c550102f after 9ca94f18, and habu-compact-the-effect-130fd5d0 after 974304d0.
3. habu-declare-the-surface-89e9aed0.
Design: ~/.cache/tmp/heron-arm64/design-surface.md. Census: ~/.cache/tmp/heron-arm64/size-census/.
Files: `src/core/checker.f`, `src/core/internal-mark.f`, `src/habu/aot-capture.f`, `src/habu/habu2.f`, `tools/engine-size.f`, `docs/engine-size.md`, `docs/forth.md`.
Ownership: heron (campaign lead).
