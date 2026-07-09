# TFAM 2b-i — engine-state write-protection: design scout

Dot `habu-tfam-2b-i-b98361ac`. READ-ONLY scout. Recommends the long-term mechanism
that makes the friend latch + checker-internal state unforgeable from user source
across native / `habu1` / Gforth mirrors.

## TL;DR recommendation

**Primary = Candidate C**, in this exact shape:

> A **runtime protected-range guard** at every raw write *sink* (`!`, `c!`, `+!`,
> `atomic!`, `atomic-add`, `atomic-cas`, `patch32`, `cp!`, `ndict!`, `,`, `c,`,
> `allot`, `snap-rebase`, and the syscall/FFI writers), checking the *final target
> address* against one **contiguous, relocated protected arena** that holds the
> friend latch + the checker/wordlist crown-jewel state. The guard is gated by the
> friend latch: no-op while the engine loads its canonical source (friend on),
> active and fail-closed forever after the cold-prefix seals it.

**Secondary (defense-in-depth, NOT load-bearing) = a *narrow* slice of Candidate A**:
drop from the post-seal user search order only the pure engine mutators that have
*zero* tool/test/lib use (`CHECKER-DEFTYPE/DEFLINEAR/DEFRECORD`, the registry
truncate/undefine hooks, `snap-rebase`, the `XREF-RETIRE*`/`*-DEFS-FROM` family).
This shrinks the named attack surface and improves diagnostics; it does not and
must not carry soundness.

**Candidate B (checker provenance)** applies *only* to the syscall/FFI pointer-arg
sinks (cat 5) inside *checked* code, and even there the runtime range check (C) is
the real backstop.

**Candidate D (mprotect the latch pages)** is rejected: checker-state writes and
user-source evaluation are not separable at runtime, and the existing page flip is
transient code-patching, not standing data protection.

The decisive reason C is the only sound primary: **the write sink is `!`
(unavoidable) and the target address can be *computed* from any legitimately
obtained data-region pointer (`here`, a `create`d buffer, a task TCB pointer, or
`data-base` itself) via ordinary arithmetic. Only the sink sees the real address,
so only a check *at the sink* can distinguish a latch write from a buffer write.**
Name-based (A) and type-based (B) schemes gate *how you spell the address*, never
*what address you actually store to*.

---

## 0. Ground truth: the two proven bypasses and why they matter

RCA (from the dot) proved two live bypasses from user stdin:

1. `5 CHECKER-PACKAGE-MODE !` — resolves a checker-internal `variable` by name at
   top-level interpret and stores to it.
2. `data-base <off> + 1 swap !` — leaks the region base, does arithmetic, stores.
   **Names no checker word at all.**

These are different attack classes and any mechanism must kill *both*.

### 0a. Top-level interpret is UNCHECKED — kills B as a primary

`EM-INTERPRET-FIND` (`src/habu/habu2.f:3034-3037`):

```
: EM-INTERPRET-FIND ( -- )
   9 DATA TKA-CELL LDR,  10 DATA TKL-CELL LDR,  LFIND LABEL@ BL,
   13 LUNDEF LABEL@ CBZ,
   11 BLR,  LMAIN LABEL@ B, ;
```

A found word is executed by `11 BLR,` — a bare branch-to-xt. **There is no
stack-effect check on the interpret path.** Only the `:`-compile path
(`EM-INTERPRET-COLON`, `habu2.f:2806`) runs the checker hook at publish
(`C-CALL-CHECK-DEFINER`). So `5 CHECKER-PACKAGE-MODE !` at stdin runs with the
checker completely out of the loop. Any type/provenance property (B) is a
compile-time artifact and cannot constrain the interpret path — which is exactly
the attack surface the RCA used.

### 0b. Even in *checked* code, `data-base <off> + !` type-checks — kills B again

`data-base` is `( -- addr )`, `+` is `( a a -- a )`, `!` is `( a addr -- )`. A
checked word `: EVIL data-base <off> + 1 swap ! ;` is well-typed and compiles,
inlining `BSTORE`. When `EVIL` executes, the inlined `STR` writes the latch. No
type error exists to catch — storing to *any* address is well-typed by design.
Provenance would have to taint the result of `data-base` and propagate the taint
through `+`, `*`, `/`, `and`, `or`… i.e. make *all* arithmetic provenance-aware,
and even then `data-base 0 + 8 * 8 /` launders it. Provenance-through-arithmetic
is the classic unsound hole.

**Conclusion: the write must be stopped at the sink, by target address. That is
Candidate C (runtime) or Candidate D (page fault). B and A are not sufficient.**

---

## 1. Memory layout — the crux for C and D

Register aliases (`src/habu/layout.f:3-8,25`): `DBASE`=x26 (data-base), `NDICT`=x27,
`CP`=x28, `DATA`=x20, `REGION`=`$400000` (4 MB). `dbase@` returns `$300000000`
(`test/engine-suite.f:1072`), i.e. the data region base == `RBASE-VA`. Data cells,
dictionary records, and compiled code all live in the *one* region
`[DBASE, DBASE+REGION)`; code grows above data (`cp@ dbase@ - 0 >`,
`engine-suite.f:1074`).

### 1a. The control block `$0..$4000` interleaves crown jewels with legit cells

From `src/habu/layout.f`, fixed offsets in the low control block:

| Crown jewels (must be unforgeable) | Off | Legit user-written / read cells | Off |
|---|---|---|---|
| `CUR-CELL` (current wordlist) | `$1A0` | `S0-CELL` (stack base) | `$1D0` |
| `WIDN-CELL` (next WID) | `$1A8` | `TASK-TCB-CELL` | `$3C88` |
| `HOOK-CELL` (checker xt) | `$1B0` | `TASKS-LIVE-CELL` | `$3C90` |
| `PKG-PUB-CELL`/`PKG-PRI-CELL` | `$27C0/$27C8` | FFI buffer block (`lib/ffi-abi.f`) | `$3A00..$3C88` |
| `DEFER-META`/`DEFER-XT` | `$27E0/$27E8` | `EVALERR-CELL` (read) | `$37D8` |
| `TSIG/TCSIG/CRSIG`, `DEF-WL-CELL` | `$2780…`,`$260` | — | — |

The layout comment itself (`layout.f:82-90`) spells the interleave:
`$3A00..$3C88` = `lib/ffi-abi.f` FFI buffers, `$3C88..$3CA0` = task cells,
`lib/task.f` grows `TASK-USER-BASE` up from `$3D00`. **Crown jewels and legit
user-writable cells are interleaved at 8-byte granularity in the same block.** A
single `[base,end)` range cannot separate them *without relocation* — this is the
central implementation fact for C.

### 1b. `CHECKER-PACKAGE-MODE` is not even in the control block

`checker.f:3418-3420` allocates `CHECKER-PACKAGE-NAME`/`-U`/`-MODE` with
`create`/`variable` → `here`+`allot`, landing *above* `$4000` (`DATA-START`,
`layout.f:130`) interleaved with everything else loaded during engine boot, and
below the user libs that `allot` on top of it. So today the checker's own state is
scattered across the general dictionary-data area, not a protected band.

### 1c. Consequence: C needs a relocated arena; A/B/D do not fix the address space

Because the address space is flat and computable, and crown jewels are interleaved
with legit cells, the sound design is: **relocate every friend/checker crown-jewel
cell + the latch into one contiguous protected arena** (either a fixed sub-band of
the region or a separately-mapped region with its own reserved base register), and
range-check sinks against exactly that arena. The checker registries the plan adds
(TFAM/SUMV/SCHEMA) are already arena/relocatable ("grow/rebase/snapshot pattern",
PLAN item 2/3), so they are born in the protected arena for free; the migration is
the *fixed* cells (`CUR/WIDN/HOOK/PKG/DEFER/TSIG`) and the new latch.

---

## 2. The write sinks and their cost (Candidate C detail)

### 2a. Two-tier prim structure

`FPRIM` (`habu1.f:60`) wraps a body with an x30 frame (non-leaf, *called*);
`FPRIM-L` (`habu1.f:69`) is a leaf whose body is emitted with no frame and is
*inlined* by the compiler. `!`,`c!`,`+!`,atomics,`here`,`,`,`c,` are `FPRIM-L`
(`habu1.f:1703-1707,1718-1719`) → **inlined at every checked call site**;
`patch32`, `ffi-call*`, `execute`, `compile,` are `FPRIM` (`habu1.f:1720-1764`).

`BSTORE` today (`habu1.f:1185-1186`):
```
: BSTORE ( -- )  B G-POP A G-POP  A B 0 STR, ;
```
After register allocation: pops (vstack→reg, cheap) + one `str x_a,[x_b]`.

### 2b. The guard, tightest form (~3-4 instrs, no memory load)

Reserve one register `PLEN` (like `DBASE`/`NDICT`/`CP` are reserved) holding the
*effective* protected length: `0` before seal, the real length after. Put the arena
at a fixed offset `PROT_OFF` from `DBASE`. Guarded `BSTORE`:

```
sub  tmp, B, DBASE          ; DBASE already live in x26
sub  tmp, tmp, #PROT_OFF     ; PROT_OFF build-time immediate
cmp  tmp, PLEN               ; unsigned
b.lo <trap-fail-closed>      ; in [arena, arena+PLEN) -> trap
str  A, [B]
```

- Pre-seal `PLEN=0` ⇒ `cmp tmp,0; b.lo` never taken ⇒ engine loads freely.
- Post-seal `PLEN=arena_len` ⇒ any store into the arena traps.
- `b.lo` is statically not-taken → near-perfect branch prediction; the two ALU ops
  dual-issue. Honest cost: **~1 cycle amortized per store, +12 bytes per site.**
- No extra memory load (arena base folds into `DBASE+PROT_OFF`; `PLEN` in a reg).
  If the register budget can't spare `PLEN`, use a fixed cell (+1 hot L1 load).

The interpret attack path is *cheaper* still: interpret `!` executes the single
shared `FPRIM-L` stub via `11 BLR,` (§0a) — one guarded body covers it.

### 2c. Context that makes the cost acceptable

The engine already pays real `mprotect` syscalls + cache flushes on *every*
definition (the RW↔RX flip, §3). A 3-4 instruction compare-branch per store is
negligible against that, and stores inside hot compiled loops sit in
already-flushed code paying no per-store syscall. The guard is inline arithmetic,
not a syscall.

### 2d. Sinks that must ALL carry the guard (the "land together" set)

From census cat-3 + cat-5 (`docs/census-tfam-2b.md`): `!` `c!` `+!` `atomic!`
`atomic-add` `atomic-cas` `patch32` `cp!` `ndict!` `,` `c,` `allot` (extending into
arena) `snap-rebase`; plus every syscall/FFI writer whose pointer arg can land in
the arena (`read` `readlink` `stat64` `lstat64` `getdirentries64` `poll` `ioctl`
`mmap` remap `ffi-call*`). Native sites: `habu1.f:1185,1191,1197,1203,1205,1207,
1250,1266,1270,1274,1548,967,968` + syscall bodies `habu1.f:1307-1541`. Miss any
one and it becomes the new bypass — this is why the dot says latch + protection
must land together. Syscall/FFI writers are where B-style provenance *does* help
in checked code, but the runtime range check on the buffer pointer is the
interpret-path backstop.

### 2e. Gforth mirror (`bootstrap/cg/forth.fs`) — subset, prove-absence for the rest

Guarded sinks present in stage0: `BSTORE`(437) `BPLUSSTORE`(441) `BCSTORE`(445)
`BHERE`(466) `BALLOT`(479) `BCOMMA`(481) `BCCOMMA`(483) `BPATCH32`(530)
`BCPSET`(252) `BNDSET`(253). **Absent (parity = prove they cannot exist in
stage0):** atomics, `snap-rebase`, `ffi-call*`, `readlink/stat64/lstat64/
getdirentries64/poll`, all `XREF-*`, all `CHECKER-*`, the `package` system. So the
gforth guard set is exactly the ~10 sinks above + the friend-latch flip; the rest
is an absence proof, not a matching guard.

---

## 3. Why Candidate D (mprotect) is rejected

### 3a. The existing flip is transient code-patching, not standing data protection

`LPROT` (`habu1.f:1921-1923`) `mprotect(DBASE, REGION, prot)` with `prot` set by the
caller. Every call site is a *paired* `RW(3) … patch … RX(5) … flush` around a code
write (e.g. `EMIT-DOESPATCH` `habu2.f:1404/1413`; `C-BP-RESTORE-ONESHOT`
`habu2.f:220-224`; the ~30 sites in the LPROT grep). It exists for icache coherency
when patching instructions, **not** to make data read-only during execution.
Empirically the region is RW when interpret executes a store (that is *why* the RCA
`!` succeeds). So there is no standing RO window to piggyback the latch onto.

### 3b. Checker writes and user evaluation are not separable

Checking a user `:` definition *is* triggered by evaluating user source, and the
checker mutates its arenas (USIG, registries, rollback frames) during that
evaluation. There is no runtime window where "user code runs" while "checker state
is idle and sealable." To use page protection you would `mprotect(RW)` before each
checker mutation and `mprotect(RO)` after — a syscall (~1 µs) per candidate probe,
on the hottest checking path, catastrophic and non-atomic. And it still leaks:
immediate words / `evaluate` executing *during* the RW-compile window could write
the arena. The runtime address guard (C) has no such mode-dependence — it is gated
only by the monotonic seal latch, not by compile-vs-execute state.

### 3c. Signal + stage0 story

Turning `SIGSEGV`/`SIGBUS` into a fail-closed throw needs a Forth signal handler;
`bootstrap/cg/forth.fs` has no mprotect-per-write or signal machinery (census
cat-3/5 absence). D cannot reach mirror parity.

---

## 4. Why Candidate A alone is insufficient, and its real migration cost

### 4a. A cannot stop computed access (bypass #2)

Hiding `CHECKER-PACKAGE-MODE` closes bypass #1, but bypass #2 names no engine word.
To close it, A would have to hide *every* source of a data-region pointer:
`data-base`, `dbase@`, `here`, `cp@`, `rbase`, and even the address of any
`create`d buffer (arithmetic off it reaches the arena). `here`/`create`/`,`/`allot`
are fundamental — you cannot hide them without breaking all memory-using code. A
gates *spelling*, not *address*. Therefore A can never be the soundness layer.

### 4b. A's migration burden is large and hits legitimate tooling

`data-base`, `dbase@`, `rbase`, `cp@`, `search-wl`, `get-current`, `wordlist`,
`XREF-*`, `LATEST`, `undefine`, `set-check`, atomics, `patch32` are used as LIVE
calls by real user-source files loaded through `--load`:

- `lib/task.f` — `data-base TASKS-LIVE-CELL + !`, `dbase@`, `rbase reg RBASE-CELL + !`,
  `ndict@`, `atomic!`, `patch32` (scheduler/TCB runtime; :157,160,193-205,218-219,251,402).
- `lib/ffi-abi.f:29-47` — `data-base FFI-*-OFF +` (FFI buffer addressing).
- `lib/ffi.f:124`, `lib/prelude.f:6`, `lib/ptx/header.f:21` — `parse-name`.
- `lib/memory-test.f:82,84,96,99` — `here data-base -`.
- `test/gate-common-lib.f:443-450,484-487` — `get-current`/`set-current`,
  `ndict@`/`ndict!`, `data-base S0-CELL + !` (swaps the eval stack base).
- `test/engine-suite.f` — `dbase@`, `cp@`, `ndict@`, `patch32`, `data-base HOOK-CELL + @`,
  `data-base DEF-TKA-CELL +` (MULTI-ERR-ORIGIN!).
- `test/prop-test-core.f:22,180,182,281` — `' HOOK set-check` / `0 set-check`
  (installs a custom checker hook — HOOK-CELL); `MARK/FORGET` use `cp@/ndict@/ndict!/cp!`.
- `test/gate-aot-negative-lib.f:8`, `test/engine-suite.f:1191,1206,1234` — `set-check`.
- `test/atomics-smoke.f`, `lib/task-test.f`, `lib/ffi-abi-test.f`, `lib/ffi-test.f`,
  `tools/hb-build-test.f` — atomics / `patch32`.
- `tools/xref-test.f` — `LATEST`, `XREF-WORDLIST/-NAME$`, `search-wl`.
- `tools/jitdump-core.f:31` — `get-current search-wl`.
- Dict-introspection live-call files that would break under a broad A (16):
  `test/gate-common-lib.f`, `test/gate-dictionary-lib.f`, `test/prop-test-core.f`,
  `tools/asm-src-test.f`, `tools/check-all-errors-core.f`,
  `tools/duplicate-definition-lint-core.f`, `tools/duplicate-definition-lint-test-lib.f`,
  `tools/image-bytes-test.f`, `tools/imgdump.f`, `tools/jitdump-core.f`,
  `tools/object-image.f`, `tools/reserved-name-lint-core.f`,
  `tools/reserved-name-lint-test-lib.f`, `tools/trusted-inventory.f`,
  `tools/xref-test.f` (+ `tools/bootstrap.sh`, the exempt recovery launcher).

So A can only ever be applied to the *pure mutators with zero tool use*
(`CHECKER-DEFTYPE/DEFLINEAR/DEFRECORD`, registry truncate/undefine hooks,
`snap-rebase`, `XREF-RETIRE*`, `HIDE/FORGET-DEFS-FROM`) — and even some of those are
touched by `check-all-errors-core`/`duplicate-definition-lint`/`reserved-name-lint`,
so the truly-hideable set is small. That is fine: under C the *state* is protected
regardless of whether the *word* is visible, so A is pure hardening.

### 4c. Under C, the same files need NO migration

Every one of the `data-base <off> + !` / `atomic!` / `patch32` uses above targets a
cell *outside* the checker/latch arena (task TCBs, FFI buffers, `S0-CELL`,
`DEF-TKA-CELL`, code region). Keep those cells outside the protected band and they
all keep working unchanged. **C's user-source migration burden ≈ zero.** The churn
is engine-internal: retarget the fixed crown-jewel cells (`CUR/WIDN/HOOK/PKG/DEFER/
TSIG`) into the protected arena (friend code, not user code).

### 4d. One residual note: `set-check` writes `HOOK-CELL`

`HOOK-CELL` ($1B0) holds the checker xt and is a crown jewel, yet tests
legitimately install hooks via `' HOOK set-check`. Under C this is consistent: the
guard is *sink-specific*. A raw `data-base HOOK-CELL + !` goes through the guarded
`!` sink and traps; the `set-check` prim writes `HOOK-CELL` through its own
dedicated `STR` (`BSETCHECK`, `habu1.f:1632`), untouched by the `!` guard. Whether
`set-check` *itself* should be friend-gated (census hard-site #5 — a forged hook
subverts all checking) is a *separate* sealing slice, out of scope for 2b-i, and
should be tracked as its own dot. 2b-i only owns: the latch and raw-write
protection of the arena.

---

## 5. Friend-latch interaction (Candidate C)

- The **seal latch cell lives inside the protected arena.** Pre-seal `PLEN=0`; the
  guard is inert; the engine's canonical source load (friend on) initializes all
  checker/crown-jewel state through ordinary stores.
- The cold-prefix generator appends a dedicated `SEAL-FRIEND` primitive as the last
  engine action before user source. Validated chokepoints (dot + census cat-7):
  after `PFX-PROVIDE-FILES` in `EMIT-COLD-PREFIX-SHARED`/`LCOLDPFX`
  (`habu2.f:796-807`) and in `C-SOURCE-BAKED` (`habu2.f:763-779`); friend must be on
  across `PFX-LOAD-BASE-FILES` (`habu2.f:449-472`). There is no per-file origin
  signal in `include.f` (`included`/`required`/`provided`, :180-195), so the
  appended-token approach is required.
- `SEAL-FRIEND` sets `PLEN := arena_len` (and the latch cell). **The latch is
  self-protecting: once set, clearing it means writing the arena, which the guard
  now blocks** — a one-way monotonic seal. Even a user calling `SEAL-FRIEND` only
  turns protection *on* (fail-safe direction); the dangerous direction is
  structurally impossible.
- The seal survives re-entrant `evaluate` and nested frames (census cat-7,
  `habu2.f:2277` EVALD-CELL) because `PLEN`/the latch is a single global reg/cell,
  not per-frame state.
- AOT persistence: the arena + latch state must survive seed capture/restore, and
  `PLEN` must be re-established above/after restore before user allocation
  (interacts with the WID-widening acceptance, PLAN 320-324; census hard-site #1
  `EM-AOT-REGISTER-RECS habu2.f:2418` u8 WID truncation).

---

## 6. Soundness — residual bypass routes under the recommendation, each closed

| Route | Closed by |
|---|---|
| `NAME !` (bypass #1) | C guard on `!` (address in arena) + A hides pure mutators |
| `data-base off + !` (bypass #2) | C guard on `!` — address-based, spelling-agnostic |
| `here delta - !`, `create`d-buf `+ !` | C guard — same, any computed address |
| `c!`/`+!`/atomics to arena | C guard on each sink (§2d) |
| `patch32`/`cp!`/`ndict!`/`snap-rebase` | C guard on each; snap-rebase friend-only |
| syscall/FFI writes buffer into arena | C range check on pointer arg + B provenance in checked code |
| `,`/`c,`/`allot` growing into arena | C guard; `allot` bound-checked against arena |
| forged checker hook via `set-check` | OUT OF SCOPE (separate dot; note §4d) |
| immediate/`evaluate` during RW-compile window | C is mode-independent (unlike D) — still guarded |
| gforth absent sinks | prove-absence parity (§2e) |

No log-and-continue, no "mostly sealed": every sink either traps fail-closed
(`die`/`throw` with a protected-write diagnostic) when sealed, or is proven absent
in the mirror.

---

## 7. Per-candidate summary

| | Soundness | Perf | Impl size / compiler | User migration | Latch fit |
|---|---|---|---|---|---|
| **A** names | Unsound alone (computed access) | free | small per word | LARGE (§4b, 16+ files) | hides latch name only |
| **B** provenance | Unsound at interpret + launderable | checker-heavy | large (pointer-kind lattice, taint all arith) | none | doesn't see interpret |
| **C** range guard | **Sound (address-based)** | ~1 cyc/store amortized | medium: guard in ~14 sinks ×3 compilers + arena relocation | **≈ none** | latch in arena, self-sealing |
| **D** mprotect | Unsound (mode-inseparable) + signal/stage0 gaps | free hot / syscall-per-mutation | large + signal handler; no gforth | none | can't isolate write windows |

**Recommendation: C primary + narrow A hardening + B only on syscall/FFI checked
sinks.** It is the only option that closes computed-address writes at both the
interpret and compiled paths, keeps every legitimate tool/lib working, meets the
fail-closed bar, and reaches gforth parity by a small guarded-sink set plus an
absence proof.
