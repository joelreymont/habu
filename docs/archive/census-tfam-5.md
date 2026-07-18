# TFAM-5 Source-Composition / Replay Census

Dot: `habu-tfam-5-ordered-4048c839` — "ordered source-event replay for preverify/tools"
(PLAN.md item 5, lines 419-561). Read-only scout output. All paths absolute-relative
to repo root `/Users/joel/Work/habu`.

PLAN item 5 in one line: replace the current path-set dependency-closure discovery
with an **ordered source-composition event log** collected by *instrumenting the
runtime loader words* (not static scanning), replay it (plus support forms) in
preverify / all-errors / public-signatures / result-cache / hb-build cache keys,
capture loader-token source spans, share one checked path-string emitter, run
discovery against a fresh/snapshotted require registry, and reject fail-closed when
the ordered artifact cannot be produced.

---

## 1. Loader words (definitions, call paths, registry, evaluate)

### Definitions — all live in `src/core/include.f`
- `src/core/include.f:180` `: included ( ptr u8 n -- )` — push depth, `INCLUDE-READ-ALL`
  (mmap+read whole file), `INCLUDE-EVALUATE`, pop, die on eval error. **Always loads;
  no registry check → replays every occurrence.**
- `src/core/include.f:186` `: required ( ptr u8 n -- )` — `INCLUDE-CHECK-PATH`;
  `REQUIRE-KNOWN?` → `2drop exit` if seen; else `REQUIRE-STORE` then `included`.
  **Exact-string dedup + load.**
- `src/core/include.f:192` `: provided ( ptr u8 n -- )` — `INCLUDE-CHECK-PATH`;
  `REQUIRE-KNOWN?` → exit; else `REQUIRE-STORE` **only** (registers path, no load).
- `src/core/include.f:197` `: include ( -- ) immediate` — `parse-name INCLUDE-CHECK-PATH included`.
- `src/core/include.f:201` `: require ( -- ) immediate` — `parse-name INCLUDE-CHECK-PATH required`.
- No `S\"`-escaped loader form exists; `s" ... "` string forms reach `required`/
  `included`/`provided` as stack strings `( ptr u8 n -- )`. `C\"`/`.\"` openers have
  no loader path (plan wants them rejected fail-closed).

### Require/provided registry (the "path-set" that must become event replay)
- `src/core/include.f:20-21` `REQUIRE-PATHS` (REQUIRE-MAX×REQUIRE-SLOT-BYTES) +
  `REQUIRE-LENS`; `:31` `variable REQUIRE-N`; cap `REQUIRE-MAX = $100` (`:9`).
- `src/core/include.f:74-107` slot/len accessors, `REQUIRE-PATH=` (byte-exact,
  length-exact → **distinct spellings are distinct entries**, `:83-91`),
  `REQUIRE-KNOWN?` (`:93-97`), `REQUIRE-STORE` (`:102-107`).
- **Registry is a persistent global; NOT snapshotted** — see category 6.

### evaluate interaction
- `src/core/include.f:177-178` `TRUSTED: INCLUDE-EVALUATE ( ptr u8 n -- ) evaluate ;`
  — the only bridge from loader → checker/interpreter.
- `src/core/include.f:174-175` `INCLUDE-EVALERR?` reads `data-base INCLUDE-EVALERR-CELL + @`
  (`EVALERR-CELL = $37D8`, `src/habu/layout.f:74`) to detect a failed nested evaluate.
- Native evaluate frame machinery (INP/INE/CP/NDICT/XDS/DP/SP save+restore):
  `src/habu/habu2.f:3275-3359` (`EM-EVAL-UNDEF-ROLLBACK`, `EM-EVAL-THROW-RECOVER`,
  `EM-EVAL-CLEAN-EXIT`). Nested `evaluate` pushes a frame and repoints INP/INE at the
  child buffer, so a running `included` body has TKA/INP inside the *child*, while the
  loader-word token lived in the *parent* buffer.

### Loader-word call sites (real usage, sample)
Repo-wide `require`/`included` lines are pervasive across `maki/`, `lib/`, `test/`,
`bootstrap/`. Representative: `test/run-worker.f:34` `path pathu included ;`;
`lib/string.f:3` `s" lib/errors.f" required`; `lib/ffi-abi.f:14` `s" lib/errors.f" required`;
hundreds of `require <path>` lines in `test/run-*.f` and `maki/*.f`.

---

## 2. Current closure consumers (how each discovers/collapses the closure)

| Consumer | Site | How it discovers today | Where the ordered event artifact plugs in |
|---|---|---|---|
| **preverify re-driver** | `src/habu/verify-source.f:373-387` (`RECORD-DEFINER?`), main loop `:389-...` | Lexes the *already-materialized single buffer*; dispatches support definers. **Does NOT recognize include/require/provided at all** — loader words are invisible to it. | Replace materialized-buffer scan with replay of the ordered event log (loader + support events) in order. |
| **check.f dep scan** | `tools/check-core.f:426-473` (`CHK-REQUIRE-TOK?`,`CHK-REQUIRED-TOK?`,`CHK-SQ-PATH$`,`CHK-SCAN-DEPS`) | **Static token scan** of `require <path>` and `s" path" required` only. **Ignores `include`/`included`/`provided`.** Recursive expansion `CHK-EXPAND-ID` `:475-492`, ordering `CHK-DEP-ORDER-PUSH`. | Consume ordered events instead of static `CHK-SCAN-DEPS`; ordered closure feeds `CHK-DEP-ORDER`. |
| **check.f materialize** | `tools/check-core.f:494-549` (`CHK-MATERIALIZE-FILE/LIST`), append `:513-521` | Flattens closure into one temp with generated `s" path" required` lines (`CHK-APPEND-REQUIRED`), writes `CHK-SRC-PATH`. Source-list mode `:536-549` loses original file identity. | Redrive originals per ordered event; stop flattening away original paths. |
| **all-errors replay** | `tools/check-all-errors-core.f:427-438` (`CA-COLLECT-SUPPORT`), `:453-458`, `:762-800` | Collects support *lines* to re-run before each colon def (`CA-SUPPORT-BEFORE` `:765-771`). Operates on the *flattened* temp buffer it was handed. **No loader-word collection.** | Support replay driven by the same ordered event log across original files. |
| **public-signatures** | `tools/public-signatures-core.f:476-566` | Single-file lex; `PS-COLLECT-EXPORTS` `:476-485` (EXPORT interning), `PS-MAYBE-TRUST-DEFINER` `:550-555`, package scope `:557-569`. No cross-file closure at all. | Load package/family metadata from shared ordered event log before rendering rows. |
| **result-cache closure lint** | `test/run-result-cache-test.f:104-189` (`REQUIRE-LINE?` `:181-183`, `REF-CHECK` `:171-179`) | Static line scan matching **`require` and `include` only** (misses `required`/`included`/`provided`). | Cross-check against discovered ordered event dependencies. |
| **hb-build cache keys** | `tools/hb-build-lib.f:453-517` (`HBB-KEY-COMMON/LINUX/MACOS/DRIVER-SOURCES`), `:720-741` (`HBB-SRC$`/ABI) | **Fully hardcoded file list** hashed into cache key; no discovery. | Ordered replay closure must feed AOT/REPL/object cache keys, not just top-level `HBB-SRC$`. |
| **srclist** | `tools/srclist.f:66-84` (`SL-PREFIX`,`SRCLIST-MAIN`) | **Fully hardcoded** prefix path string emitted verbatim. | (context; ordered replay closure is the source of truth.) |

Hardest: the preverify/all-errors consumers currently run on a *flattened materialized
buffer* and never see loader words — the redrive-each-original + ordered replay is a
structural change to how the buffer is fed, not a local patch.

---

## 3. Support-form replay — what is / is NOT replayed

### verify-source.f preverify re-driver (`RECORD-DEFINER?` `src/habu/verify-source.f:373-387`)
Replays: `package` (`:374`→`RECORD-PACKAGE :331`), `public`/`private`/`;package`
(`:375-377`), `deftype` (`:378`→`CHECKER-DEFTYPE`), `deflinear` (`:379`),
`value-record` (`:380`→`RECORD-VALUE-RECORD :358-371`), `constant` (`:381` **hardcoded
`-- a`**), `create` (`:382` `-- ptr a`), `variable` (`:383` `-- ptr a`), `defer`
(`:384`→`TRUST-DEFER`), `trusted:` (`:385`→`TRUSTED-DEFINITION`), `undefine`
(`:386`→`CHECKER-UNDEFINE`).
**NOT replayed here:** `include`/`included`/`require`/`required`/`provided`,
`immediate`, `EXPORT`, top-level bare `TRUST`.

### check-all-errors-core.f support collector (`CA-COLLECT-SUPPORT` `tools/check-all-errors-core.f:427-438`)
Collects (as replayable source lines): `package`/`public`/`private`/`;package`
(`:428-431`), `TRUSTED:` (`:432`→`CA-ADD-SUPPORT-TRUSTED :377-380`), `defer` (`:433`),
`undefine` (`:434`), `create` (`:435`), `variable` (`:436`), `constant`
(`:437`→`CA-ADD-SUPPORT-CONSTANT :357-362`, replays the *literal* `N constant NAME`
line), `TRUST` (`:438`→`CA-ADD-SUPPORT-TRUST :382-385`).
**NOT collected here:** `deftype`, `deflinear`, `value-record`, `immediate`, `EXPORT`,
and every loader word (`include`/`required`/`provided`/...).

### public-signatures constant handling
`tools/public-signatures-core.f:552` also **hardcodes `constant` as `-- a`** (and
`create`/`variable` as `-- ptr a`, `:553-554`).

### CONCRETE GAP LIST (support/composition forms not yet replayed)
1. **All source-composition loader forms** — `include`, `included`, `require`,
   `required`, `provided`, `s" ..." required/included`, `S\" ..."` escaped forms —
   replayed by **no** consumer. verify-source and all-errors don't dispatch them;
   discovery is static-scan and partial (check-core: `require`/`required` only;
   result-cache: `require`/`include` only).
2. **`immediate`** — replayed by neither verify-source nor all-errors.
3. **`EXPORT`** — read by public-signatures (`:476-485`) but **not** registered as a
   replay/support form by preverify or all-errors.
4. **`deftype` / `deflinear` / `value-record`** — replayed by verify-source but
   **missing from check-all-errors** `CA-COLLECT-SUPPORT` (all-errors asymmetry).
5. **top-level `TRUST`** — replayed by all-errors but **not** dispatched by
   verify-source `RECORD-DEFINER?`.
6. **`constant` logical shape** — verify-source `:381` and public-signatures `:552`
   fake it as one-cell `-- a`; all-errors replays the literal line (needs the literal
   value on the stack, a different behavior). Plan wants consistent treatment.
7. **`TYPEFAMILY`/`SUMTYPE`/generated-constructor metadata** — deferred to items 6/8
   (not a first-checkpoint gap, but same event/replay framework).

---

## 4. Path-string materialization (sites the shared checked emitter must own)

All of these hand-roll `s" ` + `"` + ` ` + `<raw path bytes>` + `"` with **no escaping**
of embedded quotes, backslashes, or newlines:

- `lib/source.f:91-100` `SOURCE-APPEND-PROVIDED` — emits `s" <path>" provided\n`. Raw
  `path pathu` copied (`:96`); only structural bytes are the literal `"` and space it
  adds itself. **No quote/backslash/newline escaping.** (Also `SOURCE-APPEND-SOURCE-FILE`
  `:102-106` calls it.)
- `tools/check-core.f:513-521` `CHK-APPEND-REQUIRED` — emits `s" <path>" required\n`
  into the flattened temp. Raw path.
- `tools/check-core.f:819-826` `CHK-BUILD-PREFIX` — emits `s" <label>" DIAG-FILE!` into
  a generated child program (`CHK-DQ CHK-RUN-C` around the raw label). **This is the
  DIAG-FILE! materialization site the plan calls out.**
- `lib/source-test.f:111-119` `ST-PROVIDED` — test mirror of SOURCE-APPEND-PROVIDED,
  same raw pattern (fixture site to update).
- `tools/public-signatures-test.f:65-75` `PST-FIXTURE$` — builds source with `EXPORT`
  rows; fixture that must gain quote/backslash/newline path cases (`:68`).

### Runtime DIAG-FILE! consumers (take ptr/u8/n directly — safe at call, risk is in the *materialized* `s" ... " DIAG-FILE!` text above)
- `src/core/checker.f:3344` `PRIM: DIAG-FILE!`; `src/core/checker.f:4750-4758` the
  `DIAG-FILE!` word (byte-copies into `DIAGFB`, 255 cap); `:4781` `s" <input>" DIAG-FILE!`.
- `tools/check-all-errors-core.f:709`, `:719` `CA-FILE-A@ ... DIAG-FILE!`.
- `tools/check-core.f:943` `label labelu DIAG-FILE!`.
- `src/habu/aot-lib.f:32`, `src/habu/build.f:29` `1 ARGV$ DIAG-FILE!`;
  `tools/build-fixpoint.f:840` `BF-CERT-LABEL$ DIAG-FILE!`.
- Escaping note: `DIAG-FILE!` itself does no quoting; the danger is exclusively the
  generated-source form `s" <label>" DIAG-FILE!` in `CHK-BUILD-PREFIX` where an
  unescaped quote/newline in the label would break source structure. Plan: same emitter
  + rejection policy, no separate quote-only check.

---

## 5. Span capture (interpreter/current-token position data available today)

Native DATA cells (`src/habu/layout.f`):
- `TKA-CELL = $3690` / `TKL-CELL = $3698` (`:53-54`) — **current token address + length**,
  set by the tokenizer `EMIT-TOK` (`src/habu/habu1.f:1900-1919`, stores TKA at `:1912`,
  TKL at `:1917`).
- `INP-CELL = $36A0` / `INE-CELL = $36A8` (`:55-56`) — input cursor + end; advanced per
  token (`habu1.f:1907-1919`), repointed on evaluate entry/rollback
  (`habu2.f:3281-3282,3307-3308,3354-3355`, set on eval start `habu2.f:667,729,770`).
- `DEF-TKA-CELL = $250` / `DEF-TKL-CELL` (`:120`) — **saved definition-name token**
  addr/len for diagnostics (`habu2.f:1508-1509,1558-1580`).

Line/column: **not tracked at token level.** They are *recomputed on demand* by scanning
newlines from a buffer base to a name-token pointer:
- `src/core/checker.f:4759-4780` `DIAG-ORIGIN!` / `DIAG-ORIGIN-SPAN!` — given
  `base name bl bc bb`, walks bytes counting `\n` to derive abs line/col/byte.
- Re-driver mirror (verify-source): `TOKEN-START`/`ORIGIN!`/`ABS-ORIGIN`
  (`src/habu/verify-source.f:393` `TOKEN-START @ ORIGIN!`; checker.f comment
  `:4761-4764` says native mirrors verify-source ABS-ORIGIN).

**Reachability for loader instrumentation:** A *byte* span of the current/loader token
IS reachable at runtime (`TKA-CELL`/`TKL-CELL`, and `INP-CELL` for the cursor). Native
interpret dispatch that reads these: `src/habu/habu2.f:3021-3024` `EM-INTERPRET-FIND`
(loads TKA/TKL, calls find), `habu2.f:842-869`, `habu1.f:1998-2019`. **What is missing**
to turn that into a *file* `line:col` span for an event: (a) no per-evaluate-buffer file
origin is stored anywhere runtime-reachable (origin is injected per-child only via
`DIAG-FILE!`/`DIAG-ORIGIN!` by the re-driver, not by the runtime loader), and (b) no
line/col cells — only the recompute-by-scan helper exists, which needs a base+origin the
loader path doesn't have. So current-token *byte* span is reachable; current-token
*file line/col* span is not, for arbitrary evaluate buffers. This is the span-capture
work item 5 calls "add interpreter/current-token source-span capture".

---

## 6. Tool preload leakage (fresh/snapshotted require registry)

- Registry state = `REQUIRE-N` / `REQUIRE-PATHS` / `REQUIRE-LENS`
  (`src/core/include.f:20-21,31`). Dedup is exact-string (`REQUIRE-KNOWN?` `:93-97`).
- `provided`/`required` both consult it (`:188`, `:194`); a *known* path is skipped
  (`2drop exit`), so a preloaded path silences a later user `required`/`provided`.
- **`INCLUDE-SNAPSHOT-PREPARE` (`src/core/include.f:205-211`) resets INCLUDE-FD,
  INCLUDE-BUFS-A, INCLUDE-DEPTH, INCLUDE-U, INCLUDE-RD, INCLUDE-PATH-U — but does NOT
  touch `REQUIRE-N`/`REQUIRE-PATHS`/`REQUIRE-LENS`.** There is **no save/restore** of the
  require/provided registry anywhere in the tree (grep: only include.f references
  REQUIRE-N).
- Consequence (the plan's leakage): a tool that `require`s its own dependencies (e.g.
  the check tools `require lib/errors.f`, `lib/string.f`, …, see
  `test/run-worker-diag-all-strict.f:3-26`) leaves those paths in the registry; a user
  source file's `s" lib/errors.f" required` / `... provided` then becomes a no-op and is
  hidden from discovery. Discovery must run against a fresh or snapshotted
  target-equivalent registry (save `REQUIRE-N` + slot bytes before, restore after), and
  reject fail-closed if loader words are redefined/undefined/hidden before discovery
  completes.
- Note also: `reserved-name-lint-core.f` `RNL-RESERVED-DEFINER?` (`:153-161`) does **not**
  reserve `include`/`included`/`require`/`required`/`provided`, so nothing currently
  stops source from shadowing loader words before discovery — the "reject fail-closed on
  loader-word redefinition" guard does not exist yet.

---

## Plan-cited sites verified vs. actual

All item-5 cited paths exist as described **except**:
- `src/habu/verify-source.f:330-387` is cited as "source preverify" — accurate, but note
  the cited `REQUIRE-SIGNATURE` occurrences in this file (`:173,272,304,323`) are the
  **signature `--` requirement**, NOT the loader word `require`; verify-source contains
  **no loader-word replay** despite item 5 grouping it with source-composition work.
- `src/habu/habu2.f:3021-3024` is cited under item-5 paths; it is `EM-INTERPRET-FIND`
  (native interpret-find dispatch reading TKA/TKL) — relevant to instrumenting the
  current-token/loader path, consistent with the span-capture goal.
- No cited site was missing or misdescribed beyond the verify-source `REQUIRE-SIGNATURE`
  naming collision above.
