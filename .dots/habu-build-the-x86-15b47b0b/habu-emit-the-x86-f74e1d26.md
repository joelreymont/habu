---
title: Emit the x86 dictionary index and search rows
status: closed
priority: 2
issue-type: task
created-at: "2026-09-30T09:22:47.250064+03:00"
---

Problem: I2's `OUTER:FIND` runs on `search-wl` at x86 run time, and the x86 kernel has no dictionary index or search rows. Split from K9 by the K-lane design (2026-09-30).
Acceptance: twins of `HIDX:LREBUILD`/`LHIDXADD` (`habu1.f:220-225`, `4090-4230`) and `WLFIND:LENTRY` (`habu1.f:3253-3327`); rows `search-wl` (`habu1.f:3329-3341`: refuses `OWNER-API-PRI-WID` and `DNAME-INT`) and `xref-search-wl` (`habu1.f:3345-3348`). Cases through the booted harness in `test/x86-64-kernel-engine.f` over a seeded dictionary: found, absent, a private wid refused, an interpret-only name refused; the table joins `docs/x86-64.md` `### Engine-state rows`.
Files: `src/habu/kernel-x64.f`, `test/x86-64-kernel-engine.f`, `docs/x86-64.md`.
Verify: host K3's product engine: `--load test/x86-64-kernel-engine.f`; ThinkPad: the images natively.
Depends: habu-scaffold-the-x86-9af80979 (scaffold). Hand-written through `X64ASM`.
Route: direct (x86-only files).
Ownership: krait (Intel lane).
Claim: agent=krait workspace=.jj-ws/habu-emit-the-x86-f74e1d26.

Preflight corrections (Fable, 2026-09-30; these override the lines above where they differ):
- Seeding: a booted image starts with r14 = 0 and no records (`boot-x64.f:100-103`); its region is RW (`boot-x64.f:64-68, 96-100`). Files add `test/x86-64-boot-harness.f`: `RECORD, ( ptr u8 n n n -- )` (name, wid, flags) emits code that writes record r14 at `r13 + r14*DREC` (`DREC` 48, `layout.f:199`: `[0]` code, `[16]` flags or'd with the name length, `[24]` the name inline up to `DNAME-INL` 16 bytes or, with `DNAME-EXT`, a pointer, `[40]` wid; `habu1.f:3199-3206`) and increments r14.
- Cases: found (case-folded, `habu1.f:3210-3218`), absent, wid `OWNER-API-PRI-WID` (2) refused before the search, a `DNAME-INT` record refused, and `xref-search-wl` returning the record for the same `DNAME-INT` name. Every search case runs twice: before and after the index is built.
- `LENTRY` takes the linear path while `HIDXP-CELL` is zero (`habu1.f:3182-3183`), and `LHIDXADD`/`LREBUILD` return at once then (4094, 4137). So this leaf also owns the `LHIDXBUILD` twin (`habu1.f:4106-4135`: mmap `HIDX-BYTES` with `MAP-ANON-PRIVATE` (`sys.f:31`), store `HIDXP-CELL`, zero `HIDX:CLAIMS`, rebuild; `hb: dictionary index alloc failed` exits 74) and `HIDX:LFULL` (74). `C-HIDX-HASH` (`habu1.f:3120-3132`, FNV-1a with the same fold) and `C-HIDX-INS` (4007-4030) are the twins' shared parts.
- Publics in `X64KERNEL` for K9a's `ndict!`/`ndict-append` twins (`habu1.f:1323, 1349`): `HIDX-BUILD,`, `HIDX-ADD,` and `HIDX-REBUILD, ( -- )` emit a `call` to the twin (labels held in cells as `SPAN-CELL` is); each preserves every VM register and states its scratch clobbers. `xref-search-wl` registers through `PRIM-WID` with `ENGINE-PRIMS:GLOBAL-INT-WID` (`primitive-registry.f:85`).
- ARM64 lines on master: HIDX cells 189-194, `WLFIND:EMIT` 3166-3240, `BSWL` 3242-3254, `BCOMPILERSWL` 3256-3259, `EMIT-HIDX` 4082-4199; `DICT-WL:RETIRED` `layout.f:227`, `DNAME-INT` `layout.f:316`.
- Files: `src/habu/kernel-x64.f`, `test/x86-64-boot-harness.f`, `test/x86-64-kernel-engine.f`, `docs/x86-64.md`. Tests are single positive images. Host engine: master's product `5f4d3321…`.
