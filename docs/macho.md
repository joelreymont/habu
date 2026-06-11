# Minimal dynamic Mach-O for habu (macOS ARM64)

Spec for `src/cg/macho.fs` (Phase 0.1). Static binaries are SIGKILLed by AMFI
(proven); the artifact must be **dynamic** (dyld-loaded) and **ad-hoc signed**,
but contains **zero C** — its code is emitted `svc`/native. Reference generated
with `clang -arch arm64 -nostartfiles -e _main` over an svc-only `exit(42)` stub;
runs (rc 42). Regenerate: see `bench/`-style stub in this doc's history.

## Header (`mach_header_64`, 32 bytes)
| field | value |
| ----- | ----- |
| magic | `0xFEEDFACF` (MH_MAGIC_64) |
| cputype | `0x0100000C` (CPU_TYPE_ARM64) |
| cpusubtype | `0x00000000` (ALL) |
| filetype | `2` (MH_EXECUTE) |
| ncmds / sizeofcmds | count / total bytes of load commands |
| flags | `0x00200085` = MH_NOUNDEFS\|MH_DYLDLINK\|MH_TWOLEVEL\|**MH_PIE** |
| reserved | 0 |

## Load commands (clang reference emits 16; minimal runnable subset is fewer)
1. `LC_SEGMENT_64 __PAGEZERO` — vmaddr 0, vmsize `0x1_0000_0000`, no file, prot 0.
2. `LC_SEGMENT_64 __TEXT` — vmaddr `0x1_0000_0000`, **maps the header + load cmds +
   `__text`** (fileoff 0, filesize = vmsize = one page `0x4000`), initprot/maxprot
   `0x5` (r-x), 1 section `__text` (code at file offset just past the load cmds;
   ref had `__text` at offset 728, the entry).
3. `LC_SEGMENT_64 __LINKEDIT` — fileoff `0x4000`, holds fixups/symtab/signature;
   prot `0x1` (r--).
4. `LC_DYLD_CHAINED_FIXUPS` — dataoff/size into __LINKEDIT. **Likely required even
   with zero imports** (modern dyld for PIE); emit a minimal/empty chained-fixups
   blob. *Confirm empirically in 0.1c — dropping it is the first thing to test.*
5. `LC_LOAD_DYLINKER` — name `/usr/lib/dyld`.
6. `LC_MAIN` — `entryoff` = file offset of `_main` (the `__text` start); stacksize 0.
7. `LC_LOAD_DYLIB` — name `/usr/lib/libSystem.B.dylib` (satisfies AMFI; we call no
   symbol from it — code is svc-only).
8. `LC_CODE_SIGNATURE` — dataoff/size of the ad-hoc CodeDirectory in __LINKEDIT.
   **Apply via `codesign -s -` after writing** (the one tooling touch, plan-accepted)
   rather than hand-emitting the SHA-256 page hashes. codesign is "linker-signed"
   adhoc: `flags=0x20002 (adhoc,linker-signed)`, CodeDirectory `v=20400`.

Optional in the clang ref, drop unless 0.1c shows dyld needs them: `LC_DYLD_EXPORTS_TRIE`,
`LC_SYMTAB`/`LC_DYSYMTAB` (may need empty stubs), `LC_UUID` (**omit for fixpoint
determinism**, §Goal), `LC_BUILD_VERSION`, `LC_SOURCE_VERSION`,
`LC_FUNCTION_STARTS`, `LC_DATA_IN_CODE`.

## Build/test loop (Phase 0.1, no C, no FFI)
1. Compose the whole image in a Forth buffer (header → load cmds → align → `__text`
   code → __LINKEDIT).
2. Write with `create-file`/`write-file`; `chmod +x` via `system`.
3. `system" codesign -s - <file>"` (ad-hoc).
4. `system" <file>"`; capture stdout + `$?`.
*Accept (0.1c):* an `svc exit(42)` stub yields rc 42.

## Determinism (Phase G fixpoint)
Omit `LC_UUID`; zero timestamps; emit `CODE-TABLE` words in insertion order;
normalized-image diff excludes `LC_CODE_SIGNATURE` and `LC_UUID`.

## Reference dump (clang, for byte-level comparison)
ncmds 16, sizeofcmds 664; `__TEXT` vmsize `0x4000`, `__text` size `0xc`, entryoff
728; `__LINKEDIT` fileoff `0x4000` filesize 456. Use `otool -l` / `otool -h` on a
freshly built reference to byte-diff against the Forth emitter's output.
