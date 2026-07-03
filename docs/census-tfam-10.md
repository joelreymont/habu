# tfam-10 object/AOT entry-point census

Dot: `habu-tfam-10-native-9ef7cc53` (PLAN.md item 10, lines 751-818).

Plan requirement (verbatim gist, lines 782-810): a test-only object/AOT entry
seeds raw physical payload slots + an invalid tag, then calls a generated checked
`MATCH` helper instead of the zero-argument `MAIN` path. Selected dictionary
identity (package/WID/record id, **not bare name**), helper-root identity, seeded
stack cells, layout/test mode, and ABI/source digest must reach AOT closure
roots, export records, object-image selected-entry handling, object
schema/index/cache metadata, artifact-cache metadata, and content-addressed cache
keys, so a normal `MAIN` object and a preseeded bad-tag object are distinct
artifacts and object-image can start at a nonzero/non-`MAIN` entry.

---

## Category 1 — MAIN discovery / entry (how the entry xt is found; identity used)

Today the AOT entry is discovered and lowered entirely by **bare string "MAIN"**;
the root is closure word 0, and the process entry does data-restore then
`bl MAIN` then `exit(0)` with **no seeded stack cells**.

- `src/habu/aot-closure.f:45-46` — `MAIN?`: `r s" MAIN" REC-NAME=`. Entry identity
  is the **bare record name string "MAIN"** compared byte-for-byte via `REC-NAME=`
  (aot-closure.f:37-44). No package/WID/record-id discrimination. **Same-name hazard site.**
- `src/habu/aot-closure.f:108-109` — `FINDADDR`: linear scan of dict records
  (`FX @ REC @ t =`) returning the record whose code addr matches a call target.
  Used to resolve call targets to records; identity = code address (fine).
- `src/habu/aot-closure.f:110-111` — `FINDMAIN`: linear scan over `ndict@` records,
  returns the **first** `REC` where `MAIN?` fires. This is the sole entry selector.
  Returns 0 if none. A selected non-MAIN entry would replace this: it must select
  by package/WID/record id, not first-name-match. **Primary insertion point for
  selected-entry choice.**
- `src/habu/aot-closure.f:168-169` — `CLOSURE`: `FINDMAIN dup 0= IF drop
  s" aot: no MAIN" 74 die THEN dup ROOTREC ! ADD-CLO` then BFS. The `aot: no MAIN`
  fallback (line 168). ROOTREC = selected root; ADD-CLO makes it closure word 0.
  Acceptance requires: helper not stripped + no `aot: no MAIN` fallback for
  non-MAIN test entries → the root selector here must accept the helper record.
- `src/habu/aot-lib.f:159-166` — `EMIT-ENTRY`: builds the process entry stub:
  8× `SP SP 2048 SUBI,` (stack frame), `XDS SP 0 ADDI,`, `EMIT-DATA-REGION-MAP`,
  `EMIT-DATA-COPY`, then `MLBL LABEL@ BL,` (`bl MAIN`, resolved when MLBL placed),
  then `0 0 MOVZ, NR-EXIT-GROUP SYS,` (exit(0)). **This is the zero-argument MAIN
  path.** Seeded stack cells for a bad-tag helper must be materialized here
  (push raw payload slots + invalid tag onto XDS before `BL,`), and MLBL must
  target the helper root, not MAIN. Layout/test-mode + seeded cells belong here.
- `src/habu/aot-lib.f:320` — `COPY-PLANNED-BLOBS`: `WI @ 0= IF MLBL LABEL@ LBL, THEN`
  places the `MLBL` label at **closure word 0** (the root). So MLBL is bound to
  whatever ROOTREC is (currently MAIN). If ROOTREC becomes the helper, MLBL
  auto-points at the helper — but EMIT-ENTRY still needs the seeded-cell prologue.

Identity summary: entry identity today = bare name "MAIN"; root = closure word 0;
MLBL = address of closure word 0; EMIT-ENTRY = fixed zero-arg call. No seeded
cells, no test mode, no record-id/WID anywhere in the entry path.

---

## Category 2 — object pipeline identity (schema / index / cache rows; where preseed metadata is missing)

**Object schema (`HBOBJ	1`).** Emit words `lib/object.f:347-393`; parse
`lib/object.f:243-272`. Rows that exist today:
`source`(hashed) `target` `checker` `compiler` `require` `text` `data`
`package` `export` `def` `import` `type` `reloc` `noret`. **There is NO
`entry`/`root`/`main`/`seed`/`testmode`/`abi` row.** Entry selection is not stored
in the object at all — it is an AOT emit-time concept (Category 1). So a normal
`MAIN` object and a preseeded bad-tag object would be **byte-identical**, which is
the exact stale-artifact hazard the plan flags.

- `lib/object.f:243-272` — `PARSE-LINE`: the whole tag whitelist; adding a
  selected-entry/preseed row means a new tag + `EXPECT-TABS` arm here. `def`
  (267-271) and `export` (259) carry symbol identity **by name string only**.
- `lib/object.f:377-381` — `EXPORT+` (`export` = name+vis, LINE2) and `DEF+`
  (`def` = name+addr+effect, LINE3N). These are the closest existing carriers of a
  "selected symbol"; neither carries package/WID/record-id, seeded cells, or test
  mode. A selected-entry row would sit beside these.
- `lib/object.f:444-449` — `OBJ:KEY-HEX`: object **record key** = SHA256 of full
  object bytes, domain `habu-object-record-key-v1`. Pure content address of bytes.
  Because preseed/entry is not in the bytes, preseed objects collide with normal
  objects here. **Preseed/entry/ABI digest must feed this hash (or a new row must
  be added so the bytes differ).**
- `lib/object-resolve.f:27-45` — `SOURCE-KEY!` builds the source→object index key
  from (src,target,checker,compiler) only; `CHECK-HEADERS` (34-39) re-validates the
  same four. **Resolve layer** where a selected-entry/preseed/ABI dimension must be
  injected so a preseeded run cannot resolve to the normal-MAIN object.
- `lib/object-resolve.f:50-67` — `STORE`/`LOAD`: wire `OBJIDX` (source→record) +
  `OBJSTORE` (record→bytes) together; both keyed only on the four headers +
  content hash. Restore path.
- `lib/object-index.f:96-105` — `SOURCE-KEY-HEX`: content key over
  `obj-source-index-v1` + src + target + checker + compiler. **No entry / preseed /
  seeded-cells / test-mode / ABI field.** This is the source-index restore key that
  the plan says must gain entry+preseed metadata.
- `lib/object-index.f:110-122` — `STORE`/`LOAD`: maps source-key path → object
  record-key (two-level index). A stale normal-MAIN record-key here would satisfy a
  preseeded source-key unless the source-key is widened.
- `lib/object-cache.f` (whole file) — `OBJSTORE` content store. `STORE` (104-110)
  writes `<KEY-HEX>.hbo` where key = `OBJ:KEY-HEX` (record content hash). `LOAD`
  (112-117) reads by that key and re-verifies the hash. Keyed purely on object-byte
  content; no entry/preseed dimension. `KEY-U=64`, suffix `.hbo`.
- `lib/object-link.f:343-365` — symbol resolution **by bare name STR=**:
  `EXP-MATCH?`/`EXP-IDX`/`EXP-FIND?` and `DEF-MATCH?`/`DEF-IDX`/`DEF-HAS?`. A
  selected export/def is found by bare name only → **same-name hazard** across
  packages/WIDs. Selecting a helper root by name here can pick the wrong symbol.
- `lib/object-link.f:752-768` — `DEF-ADDR` (idx→code addr), `RELOC-PATCH`,
  `RELOC-TARGET`, `EXPORT-FIND?`/`DEF-FIND?`. `DEF-ADDR` is what would give a
  selected entry its **code offset** for a nonzero/non-MAIN image entry.
- `tools/object-image.f:72-76` — `WRITE`: `OBJLINK:APPLY NONEMPTY-TEXT TEXT>ASM
  DRV-EMIT-IMAGE`. No selected-entry argument; the image entry is hardwired.

**Image entry point (why non-MAIN start does not exist yet).**
`src/os/macos/macho.f:88-89` `MAIN, ( entryoff -- )` writes `LC_MAIN`; called at
`macho.f:157` as `CODE-OFF MAIN,` → entry **fixed at `CODE-OFF`** (offset 0 of
code). ELF mirror `src/os/linux/elf.f:206-221` (`BUILD-ELF`/`BUILD-IMAGE`) sets
e_entry at the code base likewise. `DRV-EMIT-IMAGE` (`src/habu/driver-io.f:62-65`)
just `ASM-CODE BUILD-IMAGE` + sign + write. **"object-image can start at a selected
nonzero/non-MAIN entry" requires `MAIN,` to receive a resolved entry offset (e.g.
`DEF-ADDR` of the helper root) instead of `CODE-OFF`.**

Missing-metadata summary: (a) no object schema row for entry/preseed/test-mode;
(b) source-index key + record key omit entry/preseed/seeded-cells/ABI; (c) image
entry hardwired to `CODE-OFF`; (d) export/def resolution is by bare name.

---

## Category 3 — cache keys (`tools/hb-build-lib.f`)

Three stacked caches; **none carries entry / preseed / seeded-cells / test-mode /
selected-identity today.** A preseeded bad-tag run would restore the normal-MAIN
executable at the outermost layer.

- `HBB-ARTIFACT-KEY!` (`tools/hb-build-lib.f:743-753`) — **outermost artifact
  (executable) cache key.** Fields fed: domain `hb-build-artifact-cache-v1`;
  `HBB-MAKER-KEY-HEX` (64, toolchain digest); `strict` option
  (`HBB-STRICT`); `json` option (`HBB-JSON`); two diag-origin source files
  (`tools/diag-origin-core.f`, `tools/diag-origin.f` via `HBB-KEY-FILE+`);
  `HBB-SRC-DIGEST+` (user-source SHA256). **No entry/preseed/test-mode/ABI-of-entry.**
  Restored by `HBB-RESTORE-ARTIFACT?` (776-783) — copies cached exe directly.
  **Primary place selected-entry + seeded-cells + test-mode must enter so a
  preseeded run cannot restore a stale normal-MAIN executable.**
- `HBB-BUILD` control flow (`883-885`): `HBB-PREPARE-ARTIFACT-CACHE` →
  `HBB-RESTORE-ARTIFACT?` (exe hit) → `HBB-OBJECT-HIT?` (object hit) → build. Both
  restore paths are hit *before* any codegen, so both keys must gate on preseed.
- `HBB-OBJECT-LOAD?` (`785-788`) — object restore: `OBJRES:LOAD` keyed on
  (`HBB-SRC-HEX` 64, `HBB-TARGET-ABI$`, `HBB-CHECKER-ABI$`, `HBB-COMPILER-ABI$`).
  Only 4 headers; **no entry/preseed dimension.** This is where a preseeded source
  must resolve to a different object than the normal-MAIN source.
- `HBB-BUILD-OBJECT-RECORD` (`796-804`) — **hardwires the entry as bare "MAIN":**
  line 803 `s" MAIN" s" --" OBJ:EXPORT+`, line 804 `s" MAIN" 0 s" --" OBJ:DEF+`
  (name "MAIN", addr 0, effect "--"). **Cleanest insertion point for object-level
  selected-entry identity + seeded cells + test-mode**: a preseed record would
  emit the helper's package/WID/record-id export+def (nonzero addr) plus a new
  seed/testmode row, changing the object bytes and therefore `OBJ:KEY-HEX`.
- `HBB-STORE-OBJECT` (`811-819`) — builds+validates+stores object via `OBJRES:STORE`.
- `HBB-WRITE-OBJECT` (`821-832` incl. `HBB-OBJECT-HIT?`) — on object-cache hit,
  re-links the cached object and writes the image via `OBJIMG:WRITE`; `HBB-OBJECT-HIT?`
  is the fast path that must also honor preseed/entry (else a stale MAIN object
  satisfies a preseeded run).

Supporting key helpers (context):
- `HBB-MAKER-KEY!` (`519-527`): domain `hb-build-maker-cache-v2` + engine +
  `HBB-KEY-LOAD-FILES` + common/target/driver sources → toolchain digest
  (`HBB-MAKER-KEY-HEX`). Feeds both ABI strings and the artifact key.
- `HBB-CHECKER-ABI$`/`HBB-COMPILER-ABI$` (`730-736`): `checker-effect-v1` /
  `hb-arm64-v1` each `+` maker-key-hex. `HBB-TARGET-ABI$` (`738-741`):
  `linux-aarch64`/`macos-aarch64`. `HBB-SRC-HEX!` (`719-720`) / `HBB-SRC-DIGEST+`
  (`714-717`): user-source SHA256. **Source + toolchain ABI captured; entry/preseed
  is the missing axis in every one of these keys.**

Insert axis summary: (1) artifact key 743-753 (+ preseed/entry/testmode);
(2) object source-key via OBJRES:LOAD args 785-788; (3) object bytes via the
MAIN export/def hardwire 803-804. All three must move in lockstep or one layer
serves a stale artifact.

---

## Category 4 — keyword / control lowering seeds (the pattern MATCH mirrors)

No `MATCH`/`ENDMATCH`/`J-MATCH` lowering exists yet (the `LFLAGMATCH` symbols in
habu2.f are an unrelated CLI-flag matcher). CASE/OF/ENDOF/ENDCASE is the exact
template. A keyword needs **five parts** in each compiler; MATCH adds a parallel
set (`LKWMATCH`/`LKWENDMATCH`, reuse or add match `OF`/`ENDOF`, `J-MATCH`/`J-OF`
family+variant token consumption + tag compare/branch chain / `J-ENDMATCH` +
invalid-tag die).

### Native (`src/habu/habu2.f`)
1. **KWDATA rows** `EMIT-KWDATA` (`903-935`): each keyword laid out as
   `LKW<X> LABEL@ LBL,  s" <kw>" BYTES,`. CASE/OF/ENDOF/ENDCASE at `908-909`.
   Runtime keyword compare reads `kwvar LABEL@ ADR,` + length via `LKWCMP`.
2. **Handler bodies** `J-CASE` (`961-962`), `J-OF` (`964-971`), `J-ENDOF`
   (`973-974` = `J-ELSE`), `J-ENDCASE` (`976-984`): emit the JIT machine code for
   each keyword (tag pop/compare/branch, patch chains). MATCH handlers mirror these.
3. **Dispatch-entry macros** `CF-ENTRY` (`2259-2265`), `CFN-ENTRY` (`2269-2274`),
   `CFB-ENTRY` (`2287-2304`), `CFBN-ENTRY` (`2309-2324`): each emits
   `0 kwvar LABEL@ ADR,  1 kwlen MOVZ,  LKWCMP LABEL@ BL,  0 CFSK LABEL@ CBZ,`
   (compare current token to keyword; skip to `CFSK` on miss), then runs the handler
   (`hxt EM-HXT-EXECUTE`) and `lmainlbl B,` back to LMAIN. `CF-ENTRY` spills the VS;
   `CFN-ENTRY` does not (loop words); `CFB/CFBN` handle a VS-resident branch
   condition. **This is the token-capture-before-normal-path chain**; MATCH needs a
   match-mode capture inserted before the normal local/keyword/literal/call path.
4. **Wiring** `EM-COMPILE-CONTROL-KEYWORDS` (`3118-3130`): binds string+labelvar+len
   +handler-xt+entry-kind per keyword, each `KEEP?`-guarded (dead-code elim). CASE
   `3127`, OF `3128`, ENDOF `3129`, ENDCASE `3130`. A new `EM-COMPILE-MATCH-KEYWORDS`
   mirrors this and is invoked from `EM-COMPILE-KEYWORDS` (`3169`).
5. **Label vars + assignment**: LKW* variables declared/assigned in habu2.f (native
   analog of the Gforth `EMIT-LABEL-CONTROL`); MATCH adds `LKWMATCH`/`LKWENDMATCH`.

### Gforth mirror (`bootstrap/cg/forth.fs`)
1. **Label variable decls** (`181-188`): `variable LKWCASE LKWOF LKWENDOF LKWENDCASE`
   … Add `LKWMATCH`/`LKWENDMATCH` here.
2. **KWDATA rows** `EMIT-KWDATA` (`1620-1627`): `LKWCASE @ LBL, s" case" …
   LKWOF @ LBL, s" of" … LKWENDOF @ LBL, s" endof"` (note `@ LBL,` vs native
   `LABEL@ LBL,`).
3. **Handlers** `J-CASE` (`1679-1680`), `J-OF` (`1682-1689`), `J-ENDOF` (`1691-1692`),
   `J-ENDCASE` (`1694-1702`) — byte-identical logic to native (note `1695` uses
   `{: cloop done :}` with `\ typed-local-lint: allow-bare-local`).
4. **Wiring** `EMIT-COMPILE-CONTROL-KEYWORDS` (`3122-3132`): `lmain LKWCASE 4
   ['] J-CASE CFN-ENTRY`, `lmain LKWOF 2 ['] J-OF CF-ENTRY`, ENDOF `3131`, ENDCASE
   `3132`. No `KEEP?` guard in bootstrap (no DCE); passes `lmain` as a local.
5. **Label assignment** `EMIT-LABEL-CONTROL` (`3373-3379`): `LBL LKWCASE ! LBL LKWOF !
   LBL LKWENDOF ! LBL LKWENDCASE !`. Add `LBL LKWMATCH !`/`LBL LKWENDMATCH !`.

### Shape tests (must be extended, and prove CASE unchanged)
- `tools/compiler-dispatch-test.f:119-139` — `CDT-TEST-BOOTSTRAP-COMPILE`:
  `SHAPE:MUST-HAVE` for `J-CASE/J-OF/J-ENDOF/J-ENDCASE` (`129-132`) and the exact
  wiring lines `LKWCASE 4 ['] J-CASE CFN-ENTRY` etc. (`133-136`), plus
  `SHAPE:COUNT=` occurrence asserts (`137-139`). MATCH adds MUST-HAVE rows here.
- `tools/bootstrap-codegen-test.f:231-237` — `BCG-TEST-BOOTSTRAP-LOCAL-SHADOW`:
  `BCG-MUST-BEFORE` ordering + `BCG-MUST-HAVE` for `J-CASE/J-OF/J-ENDOF/J-ENDCASE`
  in `bootstrap/cg/forth.fs`. MATCH adds parallel MUST-HAVE rows.

---

## Category 5 — die paths (deterministic runtime die, no normal continuation)

The reusable idiom (both mirrors) is `0 <rc> MOVZ,  NR-EXIT-GROUP SYS,` — emits
`movz w0,#rc; svc exit_group`, an **unconditional process exit with no
fall-through / no continuation**. A guarded (range/bounds-check) die is a
`<cond> <ok-label> BCOND/CBZ/CBNZ,` over that sequence; if the guard fails,
control falls into the die. The invalid-tag branch tail reuses this: after the
tag compare/branch chain covers every valid variant, the no-match fall-through
emits the unconditional die.

### Native (`src/habu/habu2.f`)
- `C-DIE-DOES` (`1197-1199`): writes the diagnostic then `0 70 MOVZ,
  NR-EXIT-GROUP SYS,` — canonical no-continuation die (exit 70).
- Guarded form `C-CALL-CHECK-DOES` (`1201-1217`): `… C-EQ good BCOND,  C-DIE-DOES
  good LBL,` — branch to `good` on valid, else fall into the die. This is the exact
  compare-then-die-or-continue shape the tag chain will emit per branch.
- `LRDIE` runtime-die tail `EM-COMPILE-UNDEF` (`3345-3348`): `… LRDIE LABEL@ CBZ,
  … LRDIE LABEL@ LBL,  0 70 MOVZ, NR-EXIT-GROUP SYS,`.
- Other die sites (same idiom, different rc): `786`(71), `1160`(74), `1862`(70),
  `2037/2057/2071`(75), `2219`(70), `427`(76, unknown target).
- Range/stack guard scaffolding: `C-BP-STACK-RANGE` (`199-203`) sets the VS range
  regs used by the VS-bounds guards.

### Gforth mirror (`bootstrap/cg/forth.fs`)
- `C-DIE-DOES` (`1903-1905`): `0 70 MOVZ, NR-EXIT-GROUP SYS,` — same as native.
- Guarded form `C-CALL-CHECK-DOES` (`1907-1922`) mirrors native; die at `1922`.
- `LRDIE` tail (`3309-3312`): `9 DATA REPLH-CELL LDR, 9 LRDIE @ CBZ, … LRDIE @ LBL,
  0 70 MOVZ, NR-EXIT-GROUP SYS,`.
- Conditional-die examples: `2609` / `2660` `13 ok CBNZ, 0 70 MOVZ, NR-EXIT-GROUP
  SYS,`. Other rc sites: `463`(76), `578`(exc), `786`(71), `1160`(74), `3179`(75),
  `3343`.

The Gforth and native die sequences are byte-for-byte identical, so the invalid-tag
tail must be emitted the same way in both to keep the fixpoint byte-identical.

---

## Category 6 — same-name hazards (bare-string lookup that can pick the wrong MAIN/helper)

**What identity exists to switch to.** The AOT dictionary record is 48 bytes
(`src/habu/aot-closure.f:25-26`): `0:code-addr 8:byte-len 16:name-len|flags
24:name|name-ptr`. There is **no package/WID/record-id field in the record** — the
only stable non-name identities available are the **code address** (offset 0) and
the **record index `k`** (`REC k`). "record id" therefore = dictionary record
index; package/WID is a *source/object-level* concept that must be carried in the
object schema (there is a `package` row today) and threaded down to a resolved
record index / code address for the native selector.

Hazard sites (bare-string equality that returns the first match):
- `src/habu/aot-closure.f:45-46` — `MAIN?` compares record name to `s" MAIN"`.
  Two records named `MAIN` (different packages/WIDs) are indistinguishable.
- `src/habu/aot-closure.f:110-111` — `FINDMAIN` returns the **first** `REC` whose
  name is `MAIN`. Root selection is first-name-wins → wrong root under duplicate
  names. **Switch to record-index/code-address selection of the chosen entry.**
- `lib/object-link.f:343-353` — `EXP-MATCH?`/`EXP-IDX` resolve an export by bare
  name `STR=`, returning the first index; `DEF-MATCH?`/`DEF-IDX` (`346-362`) do the
  same for defs. Two packages exporting the same symbol collide. Helper-root
  identity must be package/record-qualified here.
- `tools/hb-build-lib.f:803-804` — hardwires `s" MAIN"` as both the export and the
  def (addr 0). Bare-string identity with no package qualifier; the exact site that
  must emit package/WID/record-id + the selected helper root (nonzero addr).

Not a hazard (already identity-safe): `FINDADDR` (`aot-closure.f:108-109`) and the
call-target resolver `CALL-IN-CLO?`/`TGT` resolve by **code address**, not name.

Fix direction: root selection (FINDMAIN) → by record index/code address of the
chosen helper; object export/def → package-qualified name or record id; hb-build
object record → emit the selected helper identity instead of `s" MAIN"`.

---

## Summary

### Counts per category (distinct `file:line` sites mapped)
1. MAIN discovery/entry — 6 sites (aot-closure.f 45-46, 108-109, 110-111, 168-169;
   aot-lib.f 159-166, 320).
2. Object pipeline identity — 11 sites (object.f 243-272, 377-381, 444-449;
   object-resolve.f 27-45, 50-67; object-index.f 96-105, 110-122; object-cache.f
   whole; object-link.f 343-365, 752-768; object-image.f 72-76) + image-entry
   macho.f 88-89/157, elf.f 206-221, driver-io.f 62-65.
3. Cache keys — 5 plan sites (hb-build-lib.f 743-753, 785-788, 796-804, 821-832,
   883-885) + supporting 519-527, 714-720, 730-741.
4. Keyword/control lowering — native 5 parts (903-935, 961-984, 2259-2324,
   3118-3130, 3431-3434) + Gforth 5 parts (181-188, 1620-1627, 1679-1702,
   3122-3132, 3373-3379) + 2 shape tests (compiler-dispatch-test.f 119-139,
   bootstrap-codegen-test.f 231-237).
5. Die paths — native 6 idiom sites (1197-1199, 1201-1217, 3345-3348, +786/1160/
   1862/2037/2219) + Gforth 6 (1903-1905, 1907-1922, 3309-3312, 2609/2660, +463/
   578/786/1160/3179).
6. Same-name hazards — 4 hazard sites (aot-closure.f 45-46, 110-111;
   object-link.f 343-362; hb-build-lib.f 803-804) + 1 identity-safe (108-109).

### Cleanest insertion point for a selected-entry + preseed mechanism
The **object record builder `HBB-BUILD-OBJECT-RECORD` (tools/hb-build-lib.f:796-804)**
is the single narrowest seam. Today it hardwires `s" MAIN" s" --" OBJ:EXPORT+` /
`s" MAIN" 0 s" --" OBJ:DEF+`. Making the entry identity + seeded cells + test-mode
first-class here (a new `entry`/`seed`/`testmode` object row emitted beside the
export/def, carrying package/WID/record-id + seeded stack cells + layout/test mode
+ ABI digest) accomplishes three things at once:
1. It changes the **object bytes**, so `OBJ:KEY-HEX` (record content key,
   object.f:444-449) and the `OBJSTORE` cache path diverge automatically — a normal
   `MAIN` object and a preseed object become distinct artifacts with no extra key
   plumbing at the record layer.
2. The new row parses through `PARSE-LINE` (object.f:243-272) and links through
   `OBJLINK`, so `object-image` can read the selected entry + seeds from the object
   and (a) resolve the helper root via record id (not bare name) and (b) pass its
   `DEF-ADDR` offset to `MAIN,` (macho.f:88-89) for a non-`CODE-OFF` image entry.
3. It gives the AOT path (`FINDMAIN`→ selected-entry) and the seeded EMIT-ENTRY
   prologue (aot-lib.f:159-166) a source of truth.

Still required in lockstep (cannot be skipped): widen the **source-index key**
(object-index.f:96-105 / object-resolve.f:27-45) and the **artifact key**
(hb-build-lib.f:743-753) with the same entry/preseed/test-mode axis, or the
restore fast-paths (`HBB-RESTORE-ARTIFACT?` 776-783, `HBB-OBJECT-HIT?` 828-833)
will serve a stale normal-MAIN artifact before the record key is ever consulted.

### Plan-cited sites that do NOT exist as described (exact discrepancy)
No cited line number is wrong; all cited ranges resolve to the described code.
Gaps are **missing capabilities at existing citations** plus **two omissions in
the plan's native path list**:
1. `tools/object-image.f:72-76` (`OBJIMG:WRITE`) has **no selected-entry handling**
   — it is `OBJLINK:APPLY NONEMPTY-TEXT TEXT>ASM DRV-EMIT-IMAGE`; the image entry
   is hardwired to `CODE-OFF` in `src/os/macos/macho.f:157` (`CODE-OFF MAIN,`) and
   the ELF mirror `src/os/linux/elf.f:206-221`. "object-image can start at a
   selected nonzero/non-MAIN entry" is a capability to build, not an existing hook.
   (macho.f/elf.f/driver-io.f are the required entry-offset sites but are NOT in
   the plan's path list.)
2. `lib/object.f` schema has **no `entry`/`root`/`seed`/`testmode`/`abi` row** — the
   plan implies object schema metadata for entry/preseed, but only
   `source/target/checker/compiler/require/text/data/package/export/def/import/
   type/reloc/noret` exist (object.f:243-272, 347-393). The row must be added.
3. **Native label declaration + assignment is missing from the plan's native path
   list.** The Gforth mirror lists `forth.fs:181` (decls) and `forth.fs:3373`
   (`EMIT-LABEL-CONTROL`), but the native analogs — the `variable LKW*` block and
   `EMIT-LABEL-CONTROL` at **`src/habu/habu2.f:3431-3434`** (called at 3494) — are
   not cited for native. `LKWMATCH`/`LKWENDMATCH` must be declared and assigned
   there too, or the native keyword table will not resolve the new labels.
4. No `MATCH`/`ENDMATCH`/`J-MATCH` lowering exists anywhere yet (confirmed: the
   `LFLAGMATCH` symbols in habu2.f are an unrelated CLI-flag matcher). The entire
   keyword set is net-new, mirroring CASE.
