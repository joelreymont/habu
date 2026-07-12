# Mutator / Exposure Census — dot `habu-tfam-2b-sealed-1b77662c`

Scope: every concrete site that can reach a protected wordlist or checker/dictionary
memory range, for the sealed-system-package + friend-latch + provenance work in
**PLAN.md item 2** (lines 251-343, acceptance 281-336).

Three code paths are covered:
- **checker** = `src/core/checker.f` (primitive-effect axioms + special-token dispatch)
- **native** = `src/habu/habu1.f` (prim engine), `src/habu/habu2.f` (compiler/JIT),
  `src/habu/xref.f` (dict lifecycle/inspection)
- **gforth** = `bootstrap/cg/forth.fs` (stage0 mirror — a **reduced subset**)

"Effect DEFINED" sites only (axiom / lowering / mirror), not every call site, per task brief.

Key structural facts that shape sealing:
- The checker's primitive-effect table is `PRIM: … PRIM;` between `PTABLE-END`
  (`src/core/checker.f:3406`) and its start; every syscall/mem/wordlist prim's
  *type* is declared there. The checker's runtime behavior for a handful of words
  is instead special-cased (`execute`→RSEXEC, `postpone`/`compile,`/`immediate`/
  `evaluate`→UNSAFE-TOK reject).
- Native prims are `B*` colon words registered by name via `FPRIM`/`FPRIM-L`
  tables (`habu1.f:1681-1772,1864-…`; `forth.fs:628-768`). The registration line
  is the name→xt binding; the `B*` def is the lowering.
- Compiler intrinsics (`immediate`, `postpone`, `'`, `[']`, `package`, `public`,
  `private`, `create`…) are `C-*` emitters wired into the keyword table via
  `CF-ENTRY` under a `KEEP?` tree-shake guard (`habu2.f:2990-3010`;
  `forth.fs:3028-3145`). `KEEP?` means a minimized build may omit some — sealing
  must not assume presence.
- **gforth mirror is a strict subset**: it has NO atomics, NO `snap-rebase`, NO
  `ffi-call*`, NO `readlink/stat64/lstat64/getdirentries64/poll`, NO `xref.f`
  words, NO `CHECKER-*` mutators, and NO `package/public/private` system. Its
  exposure surface is small; parity claims in the acceptance list must account
  for words that simply do not exist in stage0.

---

## Category 1 — Wordlist mutation / lookup / raw record readers

### 1a. Checker effect axioms (`src/core/checker.f`)
| Site | Word | Note / reachability |
|---|---|---|
| checker.f:3335 | `PRIM: wordlist` | `( -- wid )` mints a fresh WID; user can allocate WIDs adjacent to protected ones. |
| checker.f:3336 | `PRIM: get-current` | leaks the current WID handle. |
| checker.f:3337 | `PRIM: set-current` | redirects where new defs land — can point new defs into a protected/generated WID. |
| checker.f:3338 | `PRIM: search-wl` | `( a u wid -- addr\|0 )` returns a raw dict-record address inside any WID (incl. sealed). Primary lookup leak. |
| checker.f:3339 | `PRIM: parse-name` | `( -- ptr u8 n )` raw source token; feeds tick/search-wl. |
| checker.f:3394 | `PRIM: [']` | `( -- xt )` bakes an xt for any found name (see also C-BTICK). |

`'` (tick) and `find` are **not** in the checker table; tick is a compiler
intrinsic (below) and there is **no user-facing `find` word** — lookup is the
internal `LFIND` routine (`habu1.f:2079`, `forth.fs:836`) plus `search-wl`.

### 1b. Native lowerings
| Site | Word | Note |
|---|---|---|
| habu1.f:1623 / forth.fs:581 | `BWORDLIST` (`wordlist`) | increments `WIDN-CELL`, hands out fresh WID. |
| habu1.f:1626 / forth.fs:583 | `BGETCUR` (`get-current`) | reads `CUR-CELL`. |
| habu1.f:1629 / forth.fs:585 | `BSETCUR` (`set-current`) | writes `CUR-CELL` — no validation of target WID. |
| habu1.f:1635 / forth.fs:590 | `BSWL` (`search-wl`) | dict-record scan returning raw addr. |
| habu1.f:1667 / forth.fs:615 | `BPARSE-NAME` (`parse-name`) | token cursor. |
| habu2.f:2066 / forth.fs:2237 | `C-TICK` (`'`) | LFIND → push code addr of any name. |
| habu2.f:2071 / forth.fs:2241 | `C-BTICK` (`[']`) | LFIND → bake xt of any name. |
| registration | habu1.f:1724,1770-1771 ; forth.fs:667,687-688 | name→xt bindings for the above. |

### 1c. XREF raw record readers / lookups (`src/habu/xref.f`) — native only, absent in gforth
| Site | Word | Note |
|---|---|---|
| xref.f:37 | `LATEST` | raw addr of most-recent dict record. |
| xref.f:31/34 | `XREF-REC-ADDR` / `XREF-REC` | index→raw record addr (any WID). |
| xref.f:44/47 | `XREF-CELL@` / `XREF-PTR@` | read arbitrary cell/ptr field of a record. |
| xref.f:50/53/56 | `XREF-START` / `XREF-LEN` / `XREF-FLAGS` | code start, len, flags of a record. |
| xref.f:59 | `XREF-WORDLIST` | WID of a record — enumerates sealed WIDs. |
| xref.f:71/74/78 | `XREF-INLINE-NAME` / `XREF-NAME-A` / `XREF-NAME$` | raw name pointer/bytes. |
| xref.f:136-204 | `XREF-FIND-WL(-INDEX)`, `XREF-QUAL-INDEX`, `XREF-FIND-QUALIFIED(-INDEX)`, `XREF-FIND(-INDEX)`, `XREF-FIND-CURRENT-INDEX`, `XREF-FIND-TARGET-INDEX` | qualified + unqualified lookups over any WID; `XREF-FIND-CURRENT-INDEX` calls `get-current` (xref.f:202). |
| xref.f:286, 288, 291, 298 | `XREF-WORDLIST` (word), `XREF`, `SEE`, `WORDS` | user-facing enumerators that walk all records. |

---

## Category 2 — Checker registry mutators

### 2a. Checker registry mutator implementations (`src/core/checker.f`)
| Site | Word | Note |
|---|---|---|
| checker.f:2050 | `CHECKER-DEFRECORD` | registers a product/record type sig. |
| checker.f:3455 | `CHECKER-PACKAGE` | opens a package namespace (writes `CHECKER-PACKAGE-NAME/-U/-MODE`). |
| checker.f:3459 | `CHECKER-PUBLIC` | switch to public visibility. |
| checker.f:3462 | `CHECKER-PRIVATE` | switch to private visibility. |
| checker.f:3465 | `CHECKER-END-PACKAGE` | close package. |
| checker.f:3577 | `CHECKER-USIGS-TRUNCATE-FROM` | truncate the USIG table (rollback/forget). |
| checker.f:3673 | `CHECKER-DEFER` | register a deferred word. |
| checker.f:3676 | `CHECKER-USIG-ADD` | add a raw user-signature (the core registry write; used by `TRUST` at 4787). |
| checker.f:3904 | `CHECKER-UNDEFINE` | remove a sig from the registry. |
| checker.f:3910 | `CHECKER-DEFTYPE` | register an opaque type. |
| checker.f:3913 | `CHECKER-DEFLINEAR` | register a linear type. |
| axioms | checker.f:3355-3364 (`PRIM: CHECKER-USIGS-TRUNCATE-FROM/UNDEFINE/DEFTYPE/DEFLINEAR/DEFRECORD/DEFER/PACKAGE/PUBLIC/PRIVATE/;package`) | these are declared as prims so the checker doesn't recurse; both axiom + impl live in checker.f. |

### 2b. Native call-hooks into the checker (`src/habu/habu2.f`)
| Site | Word | Note |
|---|---|---|
| habu2.f:1190 | `C-CALL-CHECKER-DEFER` | emits call to `CHECKER-DEFER`. |
| habu2.f:2834 | `C-CALL-CHECKER-PACKAGE` | emits call to `CHECKER-PACKAGE`. |
| habu2.f:2841 | `C-CALL-CHECKER-PUBLIC` | → `CHECKER-PUBLIC`. |
| habu2.f:2846 | `C-CALL-CHECKER-PRIVATE` | → `CHECKER-PRIVATE`. |
| habu2.f:2851 | `C-CALL-CHECKER-END-PACKAGE` | → `CHECKER-END-PACKAGE`. |

### 2c. Dictionary lifecycle mutators (`src/habu/xref.f`) — native only, absent in gforth
| Site | Word | Note |
|---|---|---|
| xref.f:221 | `XREF-RETIRE` | mark a record retired (raw record write). |
| xref.f:227 | `XREF-RETIRE-WL` | retire a name in a specific WID. |
| xref.f:214 | `XREF-REQUIRE-UNDEFINE` | validate + resolve undefine target. |
| xref.f:239 | `UNDEFINE-NAME` | undefine by name. |
| xref.f:245 | `UNDEFINE-FOUND` | undefine in given WID. |
| xref.f:251 | `UNDEFINE-IF-DEFINED` | conditional undefine. |
| xref.f:256 | `HIDE-DEFS-FROM` | bulk-hide defs from a mark. |
| xref.f:263 | `FORGET-DEFS-FROM` | bulk-forget (also rewinds `XREF-FORGET-CP`, i.e. CP). |
| xref.f:294 | `undefine` | user-facing wrapper (parse-name → UNDEFINE-IF-DEFINED). |

---

## Category 3 — Raw memory writes (effect-defining sites)

### 3a. Checker axioms (`src/core/checker.f`)
| Site | Word | Site | Word |
|---|---|---|---|
| 3256 | `!` | 3273 | `allot` |
| 3258 | `+!` | 3274 | `,` |
| 3260 | `c!` | 3275 | `c,` |
| 3262 | `atomic!` | 3319 | `patch32` |
| 3263 | `atomic-add` | 3320 | `snap-rebase` |
| 3264 | `atomic-cas` | 3328 | `rbase` |
| 3272 | `here` | 3329, 3330 | `cp@` / `cp!` |
| 3331 | `dbase@` | 3332, 3333 | `ndict@` / `ndict!` |
| 3334 | `data-base` | | |

`data-base`/`dbase@`/`cp@`/`rbase` are pointer/base **leaks** (provenance
sources); `!`/`c!`/`+!`/atomics/`patch32`/`,`/`c,`/`cp!`/`ndict!`/`snap-rebase`
are the **write sinks** those leaked pointers flow into.

### 3b. Native lowerings (`src/habu/habu1.f`, snap-rebase in `habu2.f`)
| Site | Word | Site | Word |
|---|---|---|---|
| 1185 | `BSTORE` (`!`) | 1266 | `BALLOT` (`allot`) |
| 1191 | `BPLUSSTORE` (`+!`) | 1270 | `BCOMMA` (`,`) |
| 1197 | `BCSTORE` (`c!`) | 1274 | `BCCOMMA` (`c,`) |
| 1203 | `BATSTORE` (`atomic!`) | 1548 | `BPATCH32` (`patch32`, RW-flip write) |
| 1205 | `BATADD` (`atomic-add`) | 956 | `BCPFETCH` (`cp@`) |
| 1207 | `BATCAS` (`atomic-cas`) | 967 | `BCPSET` (`cp!`, has `B-TASK-LIVE-GUARD`) |
| 1250 | `BHERE` (`here`) | 957, 968 | `BNDICTFETCH`/`BNDSET` (`ndict@`/`ndict!`, guard) |
| 958 | `BDBASEFETCH` (`dbase@`) | 959 | `BDATAFETCH` (`data-base`) |
| 1561 | `BRBASE` (`rbase`) | habu2.f:2688 | `BSNAPREBASE` (`snap-rebase`) |

### 3c. Gforth mirror lowerings (`bootstrap/cg/forth.fs`) — subset
| Site | Word | Site | Word |
|---|---|---|---|
| 437 | `BSTORE` | 479 | `BALLOT` |
| 441 | `BPLUSSTORE` | 481 | `BCOMMA` |
| 445 | `BCSTORE` | 483 | `BCCOMMA` |
| 466 | `BHERE` | 530 | `BPATCH32` |
| 248, 252 | `BCPFETCH`/`BCPSET` | 249, 253 | `BNDICTFETCH`/`BNDSET` |
| 250, 251 | `BDBASEFETCH`/`BDATAFETCH` | 542 | `BRBASE` |

**Absent in gforth:** `atomic!`/`atomic-add`/`atomic-cas`, `snap-rebase` — no
lowering, no registration.

---

## Category 4 — Execution / compilation sinks

| Site | Word | Note |
|---|---|---|
| checker.f:1103 (`RSEXEC`) + 1173 (`RS-TOK?` dispatch) | `execute` | typed apply of a quotation xt; the checker models it, does not reject. |
| checker.f:4791-4799 (`UNSAFE-TOK?`) + 4801 (`REJECT-UNSAFE`) | `postpone`, `compile,`, `immediate`, `evaluate`, `trust`, `set-check`, `[`, `]` | checker fail-closes these tokens today (whole-program reject, not friend-scoped). |
| habu1.f:1564 (`BEXEC`) ; reg 1720 / forth.fs:544 ; reg 664 | `execute` | `BLR xt` — arbitrary xt call sink (plan did NOT cite this; add it). |
| habu1.f:1033 (`BCOMPILE`) ; reg 1722 / forth.fs:288 ; reg 665 | `compile,` | appends a call to any xt at CP (plan seed). |
| habu2.f:1809 (`C-POSTPONE`) / forth.fs:2214 | `postpone` | LFIND+bake-xt+compile-call (plan seed). |
| habu2.f:2540 (`EM-AOT-BOOTRUN`) | boot xt sink | `LFIND`+`BLR` of captured entry names at every boot (plan-cited path). |
| habu1.f:1009 (`B-EVAL`) ; reg 1725 / forth.fs:266 ; reg 668 | `evaluate` | re-enters the outer interpreter on arbitrary source. |
| include.f:177 (`INCLUDE-EVALUATE`, TRUSTED) ; 180 `included` ; 186 `required` | file eval | evaluate wrappers used by the loader. |

Note: the 4 plan-cited "xt-based dispatch" seeds (`habu2.f:1809`,
`habu1.f:1033`, `forth.fs:2214`, `forth.fs:288`) are the **postpone/compile,**
pair, not a generic execute dispatch; the real execute lowering is `BEXEC`
(`habu1.f:1564` / `forth.fs:544`), which the plan omits.

---

## Category 5 — Syscall / FFI writers (writable pointer args)

### 5a. Checker axioms (`src/core/checker.f`, region 3282-3370)
| Site | Word | Writable arg |
|---|---|---|
| 3283 | `read` | `PE-PTR-U8` buffer (kernel writes). |
| 3284 | `ioctl` | `PE-PTR-A` arg (driver may write). |
| 3285 | `mmap` | returns pointer (remap/protection). |
| 3293 | `readlink` | `PE-PTR-U8` out buffer. |
| 3296 | `stat64` | `PE-PTR-U8` statbuf. |
| 3297 | `lstat64` | `PE-PTR-U8` statbuf. |
| 3298-3299 | `getdirentries64` | `PE-PTR-U8` buf + `PE-PTR-N` basep. |
| 3303 | `poll` | `PE-PTR-A` pollfd array (revents written). |
| 3365 / 3366 / 3367 / 3369 | `ffi-call` / `-n` / `-abi` / `-abi-r` | `PE-PTR-A` (+`-B`,`-C`) foreign pointer args. |

`write` (3321) takes `PE-PTR-U8` but it is a **read-only** source, not a writer.

### 5b. Native lowerings (`src/habu/habu1.f`; reg 1728-1766)
| Site | Word | Site | Word |
|---|---|---|---|
| 1307 | `BREAD` | 1478 | `BREADLINK` |
| 1310 | `BIOCTL` | 1507 | `BSTAT64` |
| 1313 | `BMMAP` | 1524 | `BLSTAT64` |
| 544 | `BPOLL` | 1541 | `BGETDIRENTRIES64` |
| 1346 | `BFFI-CALL` | 1397 | `BFFI-CALL-ABI` |
| 1413 | `BFFI-CALL-N` | 1401 | `BFFI-CALL-ABI-R` |

### 5c. Gforth mirror (`bootstrap/cg/forth.fs`; reg 679-683) — subset
| Site | Word |
|---|---|
| 518 | `BREAD` |
| 520 | `BIOCTL` |
| 522 | `BMMAP` |

**Absent in gforth:** `readlink`, `stat64`, `lstat64`, `getdirentries64`,
`poll`, all `ffi-call*`.

---

## Category 6 — `immediate` / `DNAME-IMM` + qualified/package publish

### 6a. immediate
| Site | Word | Note |
|---|---|---|
| forth.fs:29 | `DNAME-IMM` constant (`$1000000000000000`) | the flag bit set in a record's name-len/flags cell. |
| habu2.f:1803 (`C-IMMEDIATE`) / forth.fs:2206 | `immediate` | ORs `DNAME-IMM` into `&dict[NDICT-1]+16` via direct dict store (RW-flip). |
| habu2.f:3004 / forth.fs:3032 | keyword registration (`KEEP?`-guarded) | wires `immediate` to `C-IMMEDIATE`. |
| habu1.f:2183,2209 / forth.fs:857 | `DNAME-IMM` read sites in `LFIND` | how immediacy is dispatched (not a mutator). |

### 6b. Package / qualified-definition publish path — **habu2.f only (no gforth)**
| Site | Word | Note |
|---|---|---|
| habu2.f:2938 | `C-PACKAGE` (`package NAME`) | opens a package: allocs 2 fresh WIDs, sets `PKG-PUB/PRI-CELL`, points `CUR-CELL` at the private WID. **Primary publish-into-arbitrary-WID site.** |
| habu2.f:2957 | `C-PUBLIC` | flips `CUR-CELL` to the public WID. |
| habu2.f:2968 | `C-PRIVATE` | flips `CUR-CELL` to the private WID. |
| habu2.f:2979 | `C-END-PACKAGE` | restores parent WID. |
| habu2.f:2906 | `C-PACKAGE-ENSURE` | find-or-create the package dict record. |
| habu2.f:2878 | `C-PACKAGE-ALLOC-WIDS` | hands out `WIDN`+1,+2 (public,private). |
| habu2.f:2873 | `C-PACKAGE-NEW-PRIVATE-WID` | allocs a private WID. |
| habu2.f:2884 | `C-PACKAGE-NEW-RECORD` | writes a new package record (`C-QUALIFY-CAP` 1465), `LHIDXADD`. |
| habu2.f:2861 | `C-PACKAGE-NAME-GUARD` | rejects `:` in package token (only existing guard). |
| habu2.f:2993-2996 | keyword registration (`KEEP?`) | `package`/`public`/`private`/`;package` → the above. |

`CUR-CELL` set by `C-PACKAGE/PUBLIC/PRIVATE` is exactly `set-current`'s target;
a qualified/plain `:` then publishes into whatever `CUR-CELL` points to. Every
one of these is the qualified-definition path that can publish into an arbitrary
(incl. sealed/generated) WID and must consult the friend latch before mutating.

---

## Category 7 — Engine-load boundary & user-source entry (friend latch)

### 7a. Canonical baked source order (where the latch is SET during engine load)
| Site | What | Note |
|---|---|---|
| tools/srclist.f:66-77 (`SL-PREFIX`), 79 (`SRCLIST-MAIN`) | metabuild source order | the "canonical baked source list" the plan names. Emits env-base, roles, bytes, asm/icode/mnem, layout/sys, exec-vector/sha256/combinators, treeshake/rt/crash, image, **habu1.f, habu2.f, xref.f**, script-argv, driver-io, + driver `.f`. This ordering is what the friend flag should bracket. NB the file self-describes as `n.f`. |

### 7b. Runtime cold-prefix (engine files loaded/marked before user source)
| Site | What | Note |
|---|---|---|
| habu2.f:513 `EMIT-COLD-PREFIX` / 508 `EMIT-HOST-LOAD-PREFIX` / 449 `PFX-LOAD-BASE-FILES` / 481 `PFX-LOAD-FILES` | base files actually `included` at cold boot (layout, env-base, include, enums, exec-vector, sha256, combinators, xref). | friend flag should be live across this block; sealed after it. |
| habu2.f:625 `PFX-PROVIDE-FILES` / 619 `PFX-PROVIDE-ROW` | files only **marked** provided (util, structures, **checker**, render, check-hook, structures-effects, roles, bytes, targets, layouts, include, script-argv, enums, exec-vector, sha256, combinators, xref). | these are baked into the dict via AOT; `provided` just registers paths so user `require` is a no-op. |
| habu2.f:802 `EMIT-SOURCE` | dispatch: `STDIN? @ ? EMIT-COLD-PREFIX-SHARED C-SOURCE-STDIN : C-SOURCE-BAKED`. | the fork between stdin-mode and baked-mode source. |
| forth.fs:1249 `EMIT-COLD-PREFIX` (+1210 argv-cold, 1435, 1440 stdin/file, 1517 baked) | gforth-mirror equivalents. | stage0 mirror has the same cold-prefix shape. |
| include.f:192 `provided` / 186 `required` / 180 `included` / 197 `include` / 201 `require` / 177 `INCLUDE-EVALUATE` | the loader words. | `provided` (192) registers a path as already-loaded WITHOUT evaluating; `included`/`required` actually `evaluate` the file. The seal point sits between the last engine `included` and the first user token. |

### 7c. User-source entry points (where the latch must already be SEALED)
| Site | Entry | Note |
|---|---|---|
| habu2.f:734 `C-SOURCE-FILE-LIST` | `--load` argv file list | MODE-LOAD path; user files appended after cold prefix. |
| habu2.f:648 `C-SOURCE-PIPE` | stdin pipe (program on stdin) | reads stdin into buffer after `LCOLDPFX`. |
| habu2.f:744 `C-SOURCE-STDIN` | stdin dispatch (tty-probe → repl vs pipe vs file-list) | |
| habu2.f:722 `C-SOURCE-FAIL-REPL-DONE` / `SRC-REPL` label | interactive REPL | arms AOT-seed cell (730). |
| habu2.f:756 `C-SOURCE-BAKED` | baked `LSRC` source | non-stdin builds. |
| habu2.f `LFLAGMATCH` / `FLAGTAB-DATA` | flag parser | `--load` and `--` are user paths; build-fixpoint additionally owns the verified compiler-only `--build` path. |
| habu2.f:2277 `LEX0`/`LUN0`, EVALD-CELL | re-entrant `evaluate` | nested evaluate depth; latch must survive nesting and stay sealed. |
| forth.fs:1400-1517 (C-SOURCE-* mirror), 1480 `C-SOURCE-FAIL-REPL-DONE` | gforth-mirror entry points | same four entries. |

The single actual "seal before first user token" chokepoint is the transition
out of the cold-prefix (`LCOLDPFX`/`EMIT-COLD-PREFIX` completion) into the
appended user buffer — everything after `LCOLDPFX LABEL@ BL,` in
`C-SOURCE-PIPE`/`-FILE-PREFIX`/`-FAIL-REPL-DONE` is user origin. The engine and
user source share ONE evaluation buffer, so the latch cannot key on a file
boundary; it must be flipped by the prefix generator itself, then be
irreversible. The exception is `MODE-BUILD`: its statically certified compiler
prefix enters through `LCOLDPFXB` and contains an explicit `SEAL-FRIEND` before
the build driver; it is not an application source path.

---

## Summary

### Counts per category (distinct DEFINED sites)
- **Cat 1 (wordlist/lookup/record readers):** 6 checker axioms + 7 native lowerings
  (5 `B*` + `C-TICK`/`C-BTICK`) + 7 gforth lowerings + ~19 `xref.f` reader/lookup
  words. No user `find` word.
- **Cat 2 (checker registry mutators):** 11 checker impls (+10 axioms) + 5 habu2
  call-hooks + 9 `xref.f` lifecycle mutators. None in gforth.
- **Cat 3 (raw memory writes):** 19 checker axioms + 18 native lowerings + 15
  gforth lowerings (atomics & snap-rebase absent in gforth).
- **Cat 4 (exec/compile sinks):** 2 checker mechanisms (RSEXEC, UNSAFE-TOK) +
  execute/compile,/postpone/evaluate/bootrun lowerings across native+gforth
  (~11 defined sites).
- **Cat 5 (syscall/FFI writers):** 12 checker axioms + 12 native lowerings + 3
  gforth lowerings (readlink/stat64/lstat64/getdirentries64/poll/ffi absent in
  gforth).
- **Cat 6 (immediate + package publish):** 1 immediate lowering pair + 10-site
  package publish path — **habu2.f only, entirely absent from gforth**.
- **Cat 7 (load boundary):** 1 metabuild list + ~6 cold-prefix sites + ~8 user
  entry points, mirrored in gforth.

### 5 hardest / most surprising sites
1. **habu2.f:2427 (`EM-AOT-REGISTER-RECS`, def 2418) — WID stored as u8.**
   Record word1 packs `wid<<24` (bits 24-31), and it's re-expanded to dict `[40]`
   at restore. This is the exact u8 truncation the acceptance (320-324) says must
   be widened; any protected/generated WID > 255 silently truncates through AOT
   seed/restore. The inverse `ACAP-COMPACT-RECS` build-side packer must widen too.
2. **The friend latch has no natural boundary to hook.** Engine and user source
   share ONE evaluation buffer (`C-SOURCE-*` prepend cold prefix, append user
   text). `provided` (include.f:192) doesn't even evaluate — it just marks paths.
   There is no per-file "now entering user code" callback; the seal must be an
   engineered token/flag flip inside the prefix generator (habu2.f:774-804), and
   it must survive re-entrant `evaluate` (LEX0/EVALD-CELL, habu2.f:2277).
3. **`snap-rebase` (checker.f:3320 axiom; habu2.f:2688 lowering) rewrites arbitrary
   call-literal instructions across a region.** It takes six raw addresses and
   relocates code; acceptance (325) wants it friend-only or protected-range
   rejecting. It has NO gforth mirror, so parity means "prove it cannot exist in
   stage0", not "add a matching guard".
4. **The entire package/qualified-publish system is habu2.f-only.** `C-PACKAGE`
   /`PUBLIC`/`PRIVATE` (2938-2977) plus `CHECKER-PACKAGE`… drive `CUR-CELL`
   (= `set-current`) into freshly-minted WIDs, but `bootstrap/cg/forth.fs` has no
   `package` intrinsic and no `CHECKER-*` mutators at all. The "same protection in
   native, habu1, and Gforth" acceptance (331) cannot mean "identical code" here.
5. **`set-check` / `TRUSTED:` install an arbitrary xt as the checker hook**
   (`BSETCHECK`, habu1.f:1632 / forth.fs:587 → `HOOK-CELL`; `set-check` fail-closed
   in checker at 4794). A leaked/forged xt written here subverts all checking —
   an execution sink that is also a checker-registry bypass, straddling Cat 2/4.

### Plan-cited sites that do NOT exist as described (discrepancies)
1. **`--source-list` engine flag does not exist.** The acceptance (273-274, 302,
   335) repeatedly gates "user `--source-list` inputs", but the flag table
   (`FLAGTAB-DATA`, `LFLAGMATCH`) defines `--load`, compiler-only `--build`, and
   `--`; no `--source-list` anywhere in `src/` or `bootstrap/`. The
   only "source list" is the `tools/srclist.f` *tool* (which prints the order and
   is itself run via `--load`). The implementer must either add a `--source-list`
   flag (new MODE + FLAGTAB row + `C-SOURCE-*` branch) or the plan must be
   reworded; sealing "user `--source-list` inputs" is currently unimplementable.
2. **The 4 "xt-based dispatch" seeds are postpone/compile,, not execute.**
   `habu2.f:1809` and `forth.fs:2214` are `C-POSTPONE`; `habu1.f:1033` and
   `forth.fs:288` are `BCOMPILE` (`compile,`). The primary xt **execute** sink is
   `BEXEC` (`habu1.f:1564` / `forth.fs:544`) plus the checker's `RSEXEC`
   (`checker.f:1103`), none of which the plan cites. Not wrong, but the seed list
   is mislabeled and incomplete for execution sinks.
3. **habu2.f "registry" Paths (2418, 2540, 2685) are AOT/snapshot machinery, not
   TFAM/SUMV/SCHEMA registries.** 2418 = `EM-AOT-REGISTER-RECS` (record restore /
   the u8-WID site), 2540 = `EM-AOT-BOOTRUN` (boot xt sink), 2685 = `BSNAPREBASE`
   (snap-rebase). They are the right lines for the AOT-WID-persistence and
   snap-rebase acceptance bullets, but item 2's Work text ("add growable
   checker-owned registries") lives in `checker.f`, not at these habu2 lines.
4. **No user-facing `find` word.** The acceptance lists "`find` variants" among
   raw lookup paths (task cat 1), but the engine exposes lookup only via
   `search-wl` (prim) and the compiler-internal `LFIND` (`habu1.f:2079`,
   `forth.fs:836`), which is not a dictionary word and cannot be called from user
   source. Sealing "find" reduces to sealing `search-wl` + the `'`/`[']`
   intrinsics.
5. **gforth mirror lacks most cited surfaces.** Acceptance (313, 331) enumerates
   `atomic*`, `ffi-call*`, `stat64/lstat64/readlink/getdirentries64/poll`,
   `snap-rebase`, `XREF-*`, `CHECKER-*`, and the package system as needing
   protection "in native, habu1, and Gforth bootstrap paths" — but **none of these
   exist in `bootstrap/cg/forth.fs`** (verified by `rg -c`: 0 hits each). Parity
   for stage0 means proving absence, not adding guards.
