# `bin/hb` size RCA — byte-exact `__text` region map

Measured 2026-07-03 on the fable tip (`bin/hb` = 148855 bytes, macOS ARM64).
Supersedes the *estimates* in `.dots/habu-bisect-engine-growth-759ffd33.md`,
which were gross correlations. Every number below is measured from the
metacompiler's own emit cursor, not disassembly or delta-correlation.

## Method (reproducible)

The engine `__text` is the ARM64 assembler's `CODE` buffer
(`src/arch/arm64/icode.f`). `ASM-LEN` (`ASM-CP @ * 4`) is the exact byte
cursor into that buffer; the whole buffer becomes `__text` verbatim
(`DRV-EMIT-IMAGE`). `EMIT-FORTH` (`src/habu/habu2.f`) lays the buffer out in a
fixed order (`EMIT-CODE-SECTIONS` then `EMIT-SOURCE-BYTES`).

To map it, a temporary `SZ ( ptr u8 n -- )` probe printing `ASM-LEN` after
each `EMIT-*` sub-call was inserted into the emit path and one stdin-engine
build was run (`bin/hb --load … -- stdin`, private `HB_TMP`). The probes emit
**zero** target bytes, so the build still reached the byte-for-byte fixpoint
(final probe `=AFTER-SOURCE-TOTAL = 113448` = the exact `__text` size), which
proves the instrumentation did not perturb the measurement. The probes were
then reverted; the numbers are recorded here.

Region size = (cursor after a section) − (cursor before it). The stdin engine
is the one whose baked source `SRCN = 11899` (the other three `EMIT-FORTH`
calls in a build are stage2 self-rebuild generations, `SRCN ≈ 517 KB`).

## Mach-O container (148855 bytes)

| Part | Bytes | Note |
|---|---:|---|
| Mach-O header + load commands | 4096 | `__text` file offset |
| **`__text` (emitted engine)** | **113448** | `0x1bb28` — the payload; mapped below |
| `__TEXT` page padding | 13528 | zero fill to the `0x20000` segment boundary |
| `__DATA_CONST` | 16384 | one 16 KB page; only 16 bytes (`__got`) live |
| `__LINKEDIT` (symtab stub + code signature) | 1399 | |
| **total** | **148855** | |

~35.4 KB is container overhead (header + `__TEXT` pad + `__DATA_CONST` page +
linkedit/signature). It shrinks in page-sized (`0x4000` = 16 KB) steps as
`__text` crosses a page boundary.

## `__text` top-level map (113448 bytes, emit order)

| Region | Bytes | % | Kind |
|---|---:|---:|---|
| `EMIT-MAIN` (startup + main loop) | 70204 | 61.9% | code |
| `EMIT-PRIMITIVE-SECTIONS` | 12052 | 10.6% | code |
| **LSRC baked REPL source** | 11900 | 10.5% | data |
| `EMIT-DICT` (seed dictionary records) | 7156 | 6.3% | data |
| `EMIT-RUNTIME-SECTIONS` | 6428 | 5.7% | code |
| `EMIT-DICTIONARY-SECTIONS` | 5620 | 5.0% | code |
| `EMIT-AOT-SEED` | 88 | 0.1% | data (empty AOT probe) |

Correction to the old bisect: LSRC baked source is **11900** bytes, not the
~19 KB previously estimated.

## `EMIT-MAIN` breakdown (70204) — where the elephant lives

| Sub | Bytes | Note |
|---|---:|---|
| `EM-STARTUP` | 40724 | boot path |
| `EM-COMMENT` | 132 | `\`/`(` skip in main loop |
| `EM-INTERPRET` | 14768 | interpret-mode dispatch (colon defs, FIND, words) |
| `EM-COMPILE` | 14580 | compile-mode dispatch (~50 keywords, ops, literals) |

`EM-STARTUP` internals:

| Sub | Bytes |
|---|---:|
| EM-ENTRY-ARGS / RUNTIME-STACK / MMAP-CODE | 108 |
| EM-SEED-DICT | 140 |
| EM-SEED-AOT | 136 |
| EM-MMAP-DATA-REGION / DATA-INIT | 92 |
| EM-SNAPSHOT-RESTORE | 344 |
| **EM-STARTUP-RUNTIME-STATE** | **39904** |

`EM-STARTUP-RUNTIME-STATE` internals:

| Sub | Bytes |
|---|---:|
| runtime-state stores + crash/trap install | 296 |
| VRINIT/HIDXBUILD calls | 8 |
| **`EMIT-SOURCE`** | **39568** |
| trailing state stores | 40 |

## ROOT CAUSE — the 39568-byte cold-prefix, emitted 4× (degenerate)

`EMIT-SOURCE` (`habu2.f:779`) emits the runtime startup driver that builds the
"checker/stdlib prefix" the engine loads from the checkout at boot. It calls
the **cold-prefix trio** — `EMIT-COLD-PREFIX` + `PFX-LOAD-SCRIPT-ARGV-COLD` +
`PFX-PROVIDE-FILES` — **inline at four source-entry points**:
`C-SOURCE-PIPE` (`:650`), `C-SOURCE-FILE-PREFIX` MODE-LOAD (`:681`) and FPLAIN
(`:686`), and `C-SOURCE-FAIL-REPL-DONE` (`:731`, the tty/REPL path).

Measured trio size (one copy):

| Part | Bytes |
|---|---:|
| `EMIT-COLD-PREFIX` (→ `PFX-LOAD-BASE-FILES`, ~19 `ADR`+`BL` loader rows) | 160 |
| `PFX-LOAD-SCRIPT-ARGV-COLD` | 16 |
| **`PFX-PROVIDE-FILES`** | **9424** |
| **trio total** | **9600** |

`PFX-PROVIDE-FILES` (`habu2.f:623`) emits, for each of ~19 prefix files, a
`PFX-PROVIDE-ROW` that constructs the runtime string `s" <path> " provided\n`
**character by character**: every fixed character (`s`, `"`, space, then
`" provided\n`) is a `C-SOURCE-APPEND-CHAR` → `C-SOURCE-APPEND-X4` sequence of
~9 ARM64 instructions (**~36 bytes per literal character**). The only part
copied by a loop is the path itself (`C-SOURCE-APPEND-Z12`, from the `LP*`
path-data label). So ~14 fixed characters × 36 bytes × 19 files ≈ 9.4 KB **per
copy**, and it is duplicated 4×.

Two independent, compounding defects:
1. **No sharing.** The trio is identical logic at all four sites (only the
   PC-relative `ADR` immediates differ) but is inlined, not `BL`-shared —
   3 redundant copies = **~28.8 KB**.
2. **Per-character codegen.** The fixed `s" ` / ` provided\n` wrappers are all
   compile-time constants, yet each character is emitted as a full
   check/append instruction block instead of copied from a baked data blob —
   ~8.8 KB of the remaining copy is avoidable.

This engine has **no peephole/optimizer/tree-shake pass** for the stdin engine
(`SHAKE? = 0`), so neither defect is caught by codegen.

## Density check on the other big code regions (degeneracy ruled out)

- `EM-INTERPRET` 14768 + `EM-COMPILE` 14580 = 29.3 KB: the genuine
  interpret/compile dispatch for ~50 keywords + colon defs + number/FIND.
  No duplication found — this is the real compiler core.
- `EMIT-PRIMS` 8796 / ~84 primitives ≈ 105 bytes each: leaf prim bodies +
  prologue. Reasonable.
- `EMIT-DICT` 7156: seed dictionary records — see the dedicated Lever-1
  assessment below. Legitimate data, small.
- `EMIT-JIT` 3908, `EMIT-CREATE` 2516, `EMIT-KWDATA` 1020: no duplication.
- LSRC 11900: baked REPL/debugger/stepper source text (the
  `habu-decide-unbake-repl` dot owns this; AOT-compiling it is neutral on
  size, so it is not a size lever).

The escaped-string-literal and dict-hash features the earlier attribution
blamed are **not** the cost: `EMIT-ESC-*` = 676 bytes total, `EMIT-HIDX` +
`LHIDXBUILD` (dict hash) is sub-1 KB. Confirmed not the growth driver.

## Lever-1 assessment — baked dict-record schema (STOP-with-evidence)

A proposed lever was densifying the 48-byte `DREC` schema (`layout.f`), on the
premise that NDICT=2099 records are baked. **Measured and falsified.**

`EMIT-DICT` (`habu1.f:2241`) bakes only the **seed** records. Measured for the
stdin engine:

| Part | Bytes |
|---|---:|
| EXT name blobs (names > 16 chars, out-of-line) | 44 |
| `LNCOUNT` count cell | 8 |
| record table = **148 × 48 (`DREC`)** | 7104 |
| **`EMIT-DICT` total** | **7156** |

`LNCOUNT = #PL = 148` records baked — the primitives plus a handful of emitted
engine words. **The 2099 count is the runtime dictionary**, which lives in the
`DICT-SIZE = $61000` (397312-byte) arena reserved in the runtime code region by
`EM-SEED-DICT` (`habu2.f:2400`) and grown at startup as the checker/stdlib/REPL
prefix files are evaluated. Those 1951 non-seed words are **not** in `__text`.

So the entire densifiable surface is 7104 bytes. The record is
8 (xt) + 8 (end) + 8 (len|flags) + 16 (inline name) + 8 (trailer). The most
aggressive safe reshape (intern names to a byte-offset table, narrow
len/flags) removes at most ~16–24 bytes/record ≈ **3–3.5 KB** — and the `DREC`
stride is load-bearing for `FIND` (`habu1.f:2011`), `HIDX`
(`EMIT-HIDX`/`LHIDXBUILD`), `EM-SEED-DICT`, and every runtime dict walk, so any
change risks the byte-for-byte fixpoint and dup-check correctness. **Verdict:
~10× less payoff than the cold-prefix at far higher risk — do not pursue as the
top lever.** If the typed-dictionary-record-schema dot lands a proven-safe
reshape later, it recovers ≤3.5 KB; it is not the size fix.

## Codegen density (Lever-2) note

Bytes/word is not uniformly fat: `EMIT-PRIMS` ≈ 105 B/prim, the interpret and
compile dispatch cores are dense hand-written ARM64. The one density defect the
map exposes is the per-character literal emission inside `PFX-PROVIDE`
(Fix B below) — a targeted micropass there, not a global peephole, is the win.
A general peephole pass is out of scope and higher-risk than the two structural
fixes below.

## Reclaim plan (priority order, each its own commit + baseline bump)

| Fix | `__text` delta | file after | risk |
|---|---:|---|---|
| **A. `BL`-share the cold-prefix trio** (one labeled routine, called from the 4 sites; save/restore `x30`, keep `x9`/`x11` contract) | **−28800** | ~116 KB | low — same pattern as the escape-decoder BL-share |
| **B. Densify `PFX-PROVIDE`**: bake the constant `s" `/` provided\n` wrappers as a data blob, emit one copy loop instead of per-char append | **−8774** | ~99.7 KB | medium — touches runtime source-buffer construction |

A alone drops `__text` 113448 → ~84.6 KB, crossing two 16 KB pages, so the file
falls ~148855 → ~116 KB. A+B reaches ~75.9 KB `__text` → **~99.7 KB file**,
inside the historical 90–100 KB target. Both must hold the byte-for-byte
self-hosting fixpoint and lower `GB-SIZE-BASELINE-MACOS` in the same commit.
Feature set is unchanged (the REPL/debugger and all prefix files still load).
