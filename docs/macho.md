# Minimal dynamic Mach-O for habu (macOS ARM64)

Spec for `src/os/macos/macho.f`. Static binaries are SIGKILLed by AMFI
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

## Load Commands
1. `LC_SEGMENT_64 __PAGEZERO` — vmaddr 0, vmsize `0x1_0000_0000`, no file, prot 0.
2. `LC_SEGMENT_64 __TEXT` — vmaddr `0x1_0000_0000`, fileoff 0, filesize/vmsize
   = rounded header plus `__text`, initprot/maxprot `0x5` (r-x), one `__text`
   section at `CODE-OFF`.
3. `LC_SEGMENT_64 __DATA_CONST` — one page immediately after `__TEXT`, with
   section `__got` holding two non-lazy symbol pointers for libSystem `_dlopen`
   and `_dlsym`.
4. `LC_SEGMENT_64 __LINKEDIT` — immediately after `__DATA_CONST`, initially
   holding the chained-fixups blob; signing grows the segment to include the
   CodeDirectory.
5. `LC_DYLD_CHAINED_FIXUPS` — points at the 104-byte fixups blob in `__LINKEDIT`.
   The starts table names the `__DATA_CONST` segment offset, so dyld rewrites the
   two GOT cells before `LC_MAIN` transfers control.
6. `LC_LOAD_DYLINKER` — name `/usr/lib/dyld`.
7. `LC_MAIN` — `entryoff = CODE-OFF`; stacksize 0.
8. `LC_LOAD_DYLIB` — name `/usr/lib/libSystem.B.dylib`.
9. `LC_CODE_SIGNATURE` — inserted by `src/os/macos/sign2.f` for AOT images or by
   the native build promotion path for snapshots.

Optional clang-reference commands remain omitted for deterministic self-hosting:
`LC_DYLD_EXPORTS_TRIE`, `LC_SYMTAB`/`LC_DYSYMTAB`, `LC_UUID`,
`LC_BUILD_VERSION`, `LC_SOURCE_VERSION`, `LC_FUNCTION_STARTS`, and
`LC_DATA_IN_CODE`.

## Build/Test Loop
1. Compose the whole image in a Forth buffer (header → load cmds → align → `__text`
   code → __LINKEDIT).
2. Write with `create-file`/`write-file`; `chmod +x` via `system`.
3. Sign with the in-image signer or the target signing policy.
4. Execute the file and capture stdout plus status.
*Accept (0.1c):* an `svc exit(42)` stub yields rc 42.

## FFI Slots

`DLOPEN-SLOT` and `DLSYM-SLOT` compute the GOT cell addresses from the Mach-O
header text-size field at runtime. Snapshot generation stages the extra
`__DATA_CONST`/`__LINKEDIT` bytes in the image buffer at `CODE-OFF`, but the
chained-fixups blob records the final segment offset (`sfts`) so dyld binds the
real mapped page. The signer hashes a partial final code page when fixups make
the code limit non-page-aligned.

## Determinism (Phase G fixpoint)
Omit `LC_UUID`; zero timestamps; emit `CODE-TABLE` words in insertion order;
normalized-image diff excludes `LC_CODE_SIGNATURE` and `LC_UUID`.

## Reference Dump

The chained-fixup/GOT encoding follows a current `clang -arch arm64` reference
that imports `dlopen` and `dlsym`: `__DATA_CONST,__got` has two cells, and the
104-byte chained-fixups blob binds them to libSystem `_dlopen` and `_dlsym`.
Keep `LC_UUID` and other timestamp-like commands out of the Habu emitter so the
self-host fixpoint remains deterministic.
