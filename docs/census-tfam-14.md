# TFAM-14 Census — Enum Families + Legacy `ENUM` Migration

Dot: `habu-tfam-14-enum-f418a044`. Scope: `PLAN.md` item 14
(`PLAN.md:893-909`) and its per-item proof gate **17n** (item 17,
`PLAN.md:958-1027`; sequence `... 13 -> 17m -> 14 -> 17n -> 15 -> 17o ...`,
`PLAN.md:1031`). Normative spec: `docs/type-families.md` enum surface — §1
(`31,40`), §5, §6 (`292,326`), §7 (`389-417`), §9.3 (`517-537`), §12 (`783`),
§14 (`827`), §18 (`1127`), §22.1 (`1303`), §23 Enum (`1416-1449`), §24 (`1463-1464`),
§25.5 (`1616`), §26 Phase-3 note (`1712-1713`), §27 (`1805`).

Every claim is `file:line` + a quoted definition/snippet. Symbol names are
authoritative (quoted). Paths absolute in the summary; relative here for density
(repo root `/Users/joel/Work/habu`).

---

## 0. STATE / PREREQUISITE ALERT (read first)

Item 14 declares `Depends on: items 9-13` (`PLAN.md:906`) and sits at the END of
the chain (`... 13 -> 17m -> 14 -> 17n`, `PLAN.md:1031`). **None of the ADT
grammar/elimination/lowering machinery items 6-13 build is landed in this tree.**
Evidence:

- Grammar/eliminator words `SUMTYPE / TYPEFAMILY / VARIANT / ;VARIANT /
  ;SUMTYPE / ENUM(block) / ;ENUM / MATCH / OF / ENDOF / ;MATCH /
  construct` and the expander `PUSH-LOGICAL / LAYOUT-PUSH-FIELDS` have **zero
  definitions** anywhere in `src/ lib/ tools/`
  (`rg -l '^: SUMTYPE|^: TYPEFAMILY|^: VARIANT|^: ENUM |^: MATCH |^: ;MATCH|PUSH-LOGICAL|LAYOUT-PUSH-FIELDS' src/ lib/ tools/`
  = 0 hits).
- `dot list` (2026-07): items **6,7,8,10,11,12** are open (`o`); item **9**
  (`habu-tfam-9-construct-2dd4f2d3`), item **13** (`habu-tfaam-13-adt-5d3288f0`),
  and item **14** itself (`habu-tfam-14-enum-f418a044`) are staged. Committed
  work reaches TFAM 2b/2c seal + item-9/11 **censuses only** (`jj log`:
  `vtotuyptqnvu Add TFAM 9 implementation census`, `plzyuvxqvplo Add TFAM 11
  census`); no ADT surface source exists yet.
- **Probe (grammar unbuilt, fail-closed):**
  ```
  $ printf 'ENUM color red green blue ;ENUM\n." got here" cr\n' | bin/hb   # from repo root
  E-UNDEFINED: ENUM        # exit 70 — the block ENUM word does not exist; undefined-word reject
  ```
  So the block-style ENUM path is fail-closed today: an undefined `ENUM` token
  rejects with exit 70. Item 14 cannot land its block form before 6-13; the
  **legacy retirement/rename half is buildable now** (§4).

Consequence for the census: item 14 has two clearly separable halves —
**(A) legacy numeric `ENUM+`/`ENUM4+` migration/retirement** (dependency-free,
buildable today), and **(B) block-style `ENUM color … ;ENUM` enum families**
(pure sugar over item-6 `SUMTYPE _ 0`, gated on 6-13). This census maps both.

---

## 1. Complete inventory of today's legacy `ENUM` mechanism

### 1a. The definers — `src/core/enums.f` (11 lines, entire file)
`enums.f:1` `\ enums.f - checked legacy numeric enum counter definers.`
`enums.f:2-4`:
```
\ ENUM+/ENUM4+ thread a running counter: each defines the next name as the
\ current value and returns value+1 / value+4. The bare ENUM token is reserved
\ for the block-style ENUM ... ;ENUM type family (PLAN.md item 14).
```
`enums.f:6-7`:
```
: ENUM+ ( n -- n )
   dup create , 1 + does> ( -- n ) @ ;
```
`enums.f:9-10`:
```
: ENUM4+ ( n -- n )
   dup create , 4 + does> ( -- n ) @ ;
```

These are the **only** enum-defining words in the tree. There is NO bare block
`ENUM`/`;ENUM`, NO `ENUM4` (non-`+`) word (`rg '\bENUM4[^+]'` = 0 hits), and
NO `deftype`/`roles.f` enum path.

### 1b. How enum values are typed TODAY — **bare `n`, not nominal**
`ENUM+`/`ENUM4+` are `create , … does> @` factories: each child (`E-OK`, `CRED`,
…) is an ordinary integer word with runtime effect `( -- n )` (the inline
`does> ( -- n ) @` at `enums.f:7,10`). **There is no nominal enum type** — a
value produced by `E-OK` is an `n`, indistinguishable by the checker from any
other `n`. No `TFAM`/`SUMV` registry row is created. **Probe:**
```
$ printf '0 ENUM+ CRED ENUM+ CGRN ENUM4+ CBLU drop CRED . CGRN . CBLU . cr\n' | bin/hb  # from repo root
0
1
2                       # exit 0 — CRED=0 CGRN=1 CBLU=2, all plain n
```
So legacy `ENUM+` gives **counter-advancing named `n` constants**, nothing more.

### 1c. Every declaration site in the tree
Tree-wide word-boundary sweep `rg -rn '\bENUM4?\+' .` (excluding `.jj-ws/`,
`.jj/`) — the **only** live callers:
- **Definitions:** `src/core/enums.f:6,9` (the two definers).
- **Test/gate consumer (the ONLY consumer that executes them):**
  `test/gate-dictionary-lib.f`, word `GD-ENUMS` (`:888-916`):
  - `:891` `s" 0 ENUM+ GD-E0 ENUM+ GD-E1 ENUM4+ GD-E4 ENUM+ GD-E8 drop" GE-SRC-LINE`
    — sequence `GD-E0=0 GD-E1=1 GD-E4=2 GD-E8=6` (`ENUM4+` after value 2 → 6),
    expected out `0 1 2 6 …` (`:906-913`).
  - `:900` `s" 100 ENUM+ CODE-A ENUM4+ CODE-B drop"` inside `package GD-EV /
    public / end-package` (`:898-903`) — proves package-scoped publication.
  - `:915-916` `s" 0 ENUM+ GD-EDUP ENUM+ GD-EDUP drop"` +
    `$4E s" GD-EDUP" s" enums reject duplicate constant names" GD-RUN-BAD-SOURCE`
    — the duplicate-name reject (throw `$4E`).
- **Documentation:** `docs/forth.md:295-310` documents the surface (the
  error-code idiom `0 ENUM+ E-OK / ENUM+ E-OPEN / ENUM4+ E-RANGE / drop`,
  `:300-304`) and states `:309-310` "The bare `ENUM` token is reserved for the
  block-style `ENUM ... ;ENUM` type family." `FILEMAP.md:50` describes the
  file.

**Finding: `ENUM+`/`ENUM4+` have ZERO production consumers** (nothing under
`src/ lib/ tools/ maki/` calls them). The entire live consumer surface is the
one gate word `GD-ENUMS` plus documentation. This makes migration low-risk (§3).

### 1d. Build/cache wiring that references `enums.f` (gate-17n surface)
`enums.f` is in the core prefix/build closure — retiring or replacing it forces
lockstep edits here:
- Native prefix row: `src/habu/habu2.f` `PFX-COMMON LPENUMS s" src/core/enums.f"
  PFX-LOAD-ROW` / `PFX-PATH-ROW` (load-position label `LPENUMS`).
- Gforth mirror: `bootstrap/cg/forth.fs` `PFX-COMMON LPENUMS s" src/core/enums.f"
  PFX-LOAD-ROW / PFX-PATH-ROW / PFX-PROVIDE-ROW`.
- Fixpoint builder: `tools/build-fixpoint.f:636-637`
  `: BF-APP;ENUMS ( ptr u8 n -- ) … s" src/core/enums.f" BF-APPEND-SOURCE ;`,
  invoked in `BF-APPEND-COMMON` (`:651`) between `BF-APPEND-SCRIPT-ARGV` and
  `BF-APPEND-EXEC-VECTOR`.
- Build cache key: `tools/hb-build-lib.f` `s" src/core/enums.f" HBB-KEY-FILE+`.
- No-binary recovery launcher: `tools/bootstrap.sh` lists `src/core/enums.f`.
- Diagnose prefix: `tools/diagnose-hb-core.f` `s" src/core/enums.f" PREFIX-FILE`.
- **Result-cache closure:** `test/run-files.f:99`
  `src/core/enums.f src/core/exec-vector.f` (source-key list). Gate 17n requires
  result-cache keys updated for any new/renamed core source (`PLAN.md:971-972`).

### 1e. Consumers that switch on / compare enum values — NONE
No `case`/`of`/`=`/`MATCH` dispatch on any `ENUM+`-produced constant exists in
the tree (the values are bare `n`; the only "comparison" is the checker's
generic duplicate-name reject at define time, `enums.f` via `create`'s
dictionary check, exercised at `gate-dictionary-lib.f:915-916`). Item 14's block
enums INTRODUCE the first switch surface (`MATCH color`, §2c).

---

## 2. `TK-ENUM` family kind + what an enum family provides that legacy `ENUM` lacks

### 2a. `TK-ENUM` registry kind (declared, ZERO instances today)
`src/core/type-family.f:14-20`:
```
0 constant TK-CELL          \ scalar-cell family (no ADT layout)
1 constant TK-PRODUCT       \ record / struct
2 constant TK-SUM           \ tagged sum
3 constant TK-ENUM          \ payload-free sum
4 constant TK-EVIDENCE      \ compile-only evidence family
```
`type-family.f:18` `3 constant TK-ENUM \ payload-free sum` — an enum is
canonically a **zero-payload sum** (matches spec §9.3 "Equivalent to a
zero-payload sum", `docs/type-families.md:529-537`).

Readers/predicates:
- `type-family.f:217` `: TFAM-ENUM? ( id -- bool ) TFAM-KIND@ TK-ENUM = ;`
- `type-family.f:218-219`
  `: TFAM-LAYOUT? ( id -- bool ) {: id:n :}  id TFAM-PRODUCT? id TFAM-SUM? or id TFAM-ENUM? or ;`
  — enums ARE layout families (drive `PUSH-LOGICAL`, item 7).
- Kind stored at decl: `type-family.f:308` `arity r TF.ARITY ! kind r TF.KIND !`
  (from `TFAM-DECL` arg); default `TF.SLOTS`=0 (`:311` `0 r TF.SLOTS !`),
  `TF.TAGW`=`TAGW-CELL` (`:314`; `TAGW-CELL` = 1 cell, `:38`),
  `TF.LAYOUT`=`TL-STACK-CELL-TAG` (`:310`; `:24` default policy).
- `TFAM-KIND-VALID?` (`type-family.f:286`) admits `TK-ENUM` (0..`TK-MAX`, `:20`).

**Every registered family today is `TK-CELL`** (`type-family.f:629-643`
`TFAM-REG-CELL`, plus the reserved `field` at `:648`). `TK-ENUM` is exercised
ONLY by the registry unit tests: `test/type-family-suite.f:75`
`s" pkga" CHECKER-PACKAGE-PRIVATE s" res" 0 TK-ENUM TFAM-DECL AID !`,
`:83` `AID @ TFAM-KIND@ TK-ENUM T=`, `:92` `AID @ TFAM-ENUM? -1 T=`; and
`test/type-family-rollback-suite.f:102,117,121,125,219`. No product path builds a
`TK-ENUM` from source (no grammar).

### 2b. Variant registry (SUMV) for enum variants — populated only in tests
`type-family.f:323-334 BEGIN-STRUCTURE SUMV-REC` (`SV.FAM :324`, `SV.TAG :327`,
`SV.PAYCELLS :330`, ctor slots `SV.CTOR-SYM/PKG-OFF/PKG-U :331-333`). Creator
`SUMV-ADD` (`:375-387`, `( fam name-a name-u tag sch-start sch-count paycells --
id )`). Spec §7 pins the enum registry shape (`docs/type-families.md:389-417`):
```
family color:  arity = 0  kind = enum  max-payload-cells = 0  variant-count = 3
red:   tag = 0  payload = empty
green: tag = 1  payload = empty
blue:  tag = 2  …
```
So an enum variant is a **zero-payload** SUMV row: `SV.PAYCELLS=0`,
`SV.SCH-COUNT=0`, tag = declaration index. Tests already build this shape
(`type-family-suite.f`: `AID @ s" red" 0 0 0 0 SUMV-ADD drop`,
`AID @ s" green" 1 0 0 0 SUMV-ADD drop`). Item 14's block `ENUM` must drive
`TFAM-DECL … TK-ENUM` + one `SUMV-ADD` per variant with `paycells=0`, then
`TFAM-SLOTS! 0` (M=0) and `TFAM-VAR-RANGE!`.

### 2c. What an enum family provides that legacy `ENUM+` lacks
| Capability | Legacy `ENUM+`/`ENUM4+` | Block `ENUM color … ;ENUM` |
|---|---|---|
| Value type | bare `n` (`enums.f:7`) | **nominal `color`** (`TK-ENUM` `T-PARAM`, family-id identity) |
| `n`↔enum confusion | freely mixed (§1b probe) | **rejected** — spec §23 `docs/type-families.md:1444-1449` "`: BAD-COLOR ( -- color ) 0 ;` … because `n` is not `color`" |
| Exhaustive dispatch | none | **checked `MATCH color`** — spec §14 `:827` "Require family kind = sum or enum"; §23 `COLOR>CODE` (`:1428-1439`) |
| Duplicate variant | dup **name** reject only (`gate-dictionary-lib.f:915`) | dup **variant** reject — spec §24 `:1463` "bad enum declaration: duplicate variant red"; enforced by `SUMV-ADD` `E-TFAM-DUP` (`type-family.f:378`) |
| Missing/bad variant in match | n/a | rejected — spec §24 `:1466-1468` "unknown/duplicate/missing variant" |
| Runtime tag validation | none | **bad-tag death** — spec §25.5 `:1616` "zero-payload enum/sum layouts" (item 10) |
| Raw-tag construction | that IS the mechanism | **forbidden** — spec §12 `:783` "Do not expose public unchecked converters from `n` to enum/sum tags"; §27 `:1805` "unsafe enum casts" (non-goal) |
| Width | 1 cell (arbitrary `n`) | **tag width** — spec §18 `:1127` `WIDTH(enum) = tag width` (M=0 payload + 1 tag = 1 physical cell) |

**Critical width nuance:** an enum's physical bundle is **one cell** (the tag),
so unlike wider sums it is not multi-cell. But it is still a **hidden-field
layout row** (`@color.tag`, item 7), NOT a plain `n`. The unsoundness item 14
inherits from the missing item 7:
```
$ printf 's" " CHECKER-PACKAGE-PUBLIC s" mycolor" 0 TK-ENUM TFAM-DECL drop\n: USECOLOR ( mycolor -- n ) drop 0 ;\n." parsed one-cell" cr\n' | bin/hb
parsed one-cell         # exit 0 — mycolor parses as ONE polymorphic cell; `drop 0` type-checks (WRONG)
```
`SIG-FAM?` (`checker.f`) is kind-blind (documented in `docs/census-tfam-7.md`
§0), so a `TK-ENUM` signature is TODAY accepted as one cell and `drop` binds it.
Item 7's hidden-field reject must make `drop`/`n`-coercion on a `mycolor` fail
except through `MATCH`. **Tag 0 is a valid variant (`red`), so raw top-cell
truthiness is unsound** — spec is explicit at `docs/type-families.md:1303`
(default policy) and PLAN item 12 (`PLAN.md:813-814` "Tag 0 is a valid variant …
so raw top-cell truth is unsound"). This is why `?dup`/`if`/`case` on an enum
must reject (item 12).

---

## 3. Migration surface — per legacy site, migrated form + breakage risk

### 3a. `enums.f` definers → keep-under-legacy-name OR retire
PLAN item 14 Work (`PLAN.md:896-898`): "replace or retire the current numeric
`ENUM`/`ENUM4` chain … If compatibility is needed, move the old surface behind an
explicit legacy name and update all call sites." The naming split is **already
staged**: the source uses `ENUM+`/`ENUM4+` (with `+`), explicitly reserving bare
`ENUM` (`enums.f:3-4`, `docs/forth.md:309-310`). So **no rename of `ENUM+` is
forced** — `ENUM+`/`ENUM4+` are already "the explicit legacy name" and bare
`ENUM` is free for the block word. Two valid migrated forms:
1. **Coexist (lowest risk):** keep `enums.f` `ENUM+`/`ENUM4+` verbatim; add block
   `ENUM`/`;ENUM` in a new file (e.g. `src/core/enum-family.f`) that reuses
   item-6 `SUMTYPE _ 0`. Legacy stays a counter definer; block is the type family.
2. **Retire:** delete `enums.f`, migrate `GD-ENUMS` off `ENUM+` (or convert its
   fixture to block enums), and drop all §1d build-closure rows. Higher churn;
   only justified if `ENUM+` has no remaining users (§1c confirms none in prod).

### 3b. Block `ENUM color … ;ENUM` migrated form
Per spec §9.3 (`docs/type-families.md:517-537`) the block form desugars to:
```
SUMTYPE color 0
  VARIANT red   ;VARIANT
  VARIANT green ;VARIANT
  VARIANT blue  ;VARIANT
;SUMTYPE
```
i.e. **`ENUM` is thin sugar over item-6 `SUMTYPE` with arity 0 and zero-payload
variants** — item 14 owns only the `ENUM`/`;ENUM` token pair + the arity-0 /
`paycells=0` wiring; item 6 owns `SUMTYPE`/`VARIANT`/registry, item 8 owns the
constructors (`COLOR:RED`…), item 9 owns `MATCH`. Constructor spelling follows
Package Shape (`PLAN.md:89-102`): uppercase package+tail, so `color` → `COLOR:RED`.

### 3c. What could break
- **Persisted / serialized values:** `ENUM+` bakes a literal `n` into each child
  via `,` (`enums.f:7`), and snapshot persist stores it. Block enum variants
  assign `SV.TAG` = declaration index (`type-family.f:385`), same 0,1,2 ordering
  as `ENUM+` starting at 0 — BUT `ENUM4+` (`+4` stride) and non-zero start
  (`gate-dictionary-lib.f:900` starts at 100) have NO block-enum analog (block
  tags are always 0,1,2,…). **Any code depending on a specific non-contiguous
  numeric value (error codes `E-OK/E-OPEN/E-RANGE` at `docs/forth.md:300-302`,
  stride-4 layout) cannot migrate to block enums without a value change.** This
  is the exact "does not silently change legacy semantics" concern (Goal mapping,
  `PLAN.md:907-908`). Mitigation: keep `ENUM+`/`ENUM4+` for numeric/status
  families; use block `ENUM` only where a nominal checked type (not a specific
  integer) is wanted.
- **FFI / PTX kernel constants:** none found (`rg` for `ENUM+` callers = only
  gate + docs). No FFI boundary (`lib/ffi.f`) or PTX kernel uses `ENUM+`
  constants today, so no ABI/kernel breakage from migration. (If a future
  numeric-constant family were block-migrated, its nominal enum value would no
  longer be an `n` acceptable at an FFI/`n`-typed boundary — spec §12 `:783`
  forbids the implicit `enum→n` cast — so such families must stay `ENUM+`.)
- **`GD-ENUMS` gate:** the direct at-risk fixture. If `ENUM+` is retired,
  `gate-dictionary-lib.f:888-916` must be rewritten; if it coexists, the gate is
  unchanged and item 14 ADDS a new block-enum gate word. PLAN Risk
  (`PLAN.md:903-904`): "reusing `ENUM` without a transition will break existing
  dictionary tests at load time" — satisfied because bare `ENUM` is not currently
  a word (§0 probe), so adding it collides with nothing.
- **Build closure drift:** any `enums.f` retire/rename must update all §1d sites
  in ONE commit or the native/Gforth prefixes diverge (gate 17n stdin-manifest
  reconciliation, `PLAN.md:973-978`).

---

## 4. Dependencies — artifacts item 14 consumes; early vs waits

Item 14 `Depends on: items 9-13` (`PLAN.md:906`); transitively 6/7/8/11/12.
Per-item artifacts (all UNBUILT, §0):

- **Item 6 (`PLAN.md:509-544`):** the `SUMTYPE`/`TYPEFAMILY` defining-word grammar
  + token reservation. Item 14's `ENUM`/`;ENUM` is sugar that calls item 6's
  `SUMTYPE color 0 … ;SUMTYPE` machinery (spec §9.3 `:529-537`). Item 6
  reserves the block tokens (`PLAN.md:434-435`), leaving `ENUM`/`;ENUM` for
  item 14 to reserve at its phase (spec §26 Phase-3 `:1712-1713` "Do not reserve
  or replace `ENUM` in this phase; legacy `ENUM` is migrated in the later
  enum-family phase").
- **Item 9 (`PLAN.md:646-689`):** the checker-owned `MATCH` token protocol +
  exhaustiveness/`CF-MATCH` frames. Enum `MATCH color` requires `MATCH` to accept
  `kind = enum` (spec §14 `:827`); enum branches are zero-payload so each
  consumes only the tag (spec §23 `COLOR>CODE` `:1428-1439`). Item 14 consumes
  item 9's exhaustiveness/duplicate/unknown-variant rejects for enums
  (spec §24 `:1463-1468`).
- **Item 10 (`PLAN.md:690-741`):** native + Gforth lowering of enum constructors
  (tag push) and `MATCH` (tag compare/branch), plus the **zero-payload enum
  bad-tag runtime-death fixture** — spec §25.5 `:1615-1617` and PLAN acceptance
  `:722-724` "a zero-payload enum or sum … so fallback cleanup proves every
  payload slot plus tag is handled." Item 14's enum is precisely the
  zero-payload case item 10 must cover.
- **Item 12 (`PLAN.md:760-845`):** layout-aware stack ops. Enum is width-1 but
  still a hidden-field row; `?dup`/`if`/`case`/`depth`/`.s` must reject or
  logical-preserve the raw tag cell (`PLAN.md:813-814`, tag 0 = valid `red`).
- **Items 7/8/11/13 (transitive):** item 7 hidden-field expansion (`@color.tag`,
  M=0+1); item 8 zero-payload generated constructors (`COLOR:RED` pushes only tag,
  `M-p = 0`, spec §12); item 11 linear (enums are non-linear zero-payload, so
  trivially copyable — item 11 relevance is nil for enums); item 13 enum
  diagnostics ("bad enum declaration…", spec §24 `:1463`).

**Migratable early (dependency-free, buildable in this tree):**
- Legacy retire/rename decision (§3a) and, if chosen, the `GD-ENUMS`
  fixture + §1d build-closure edits.
- Reserving the naming convention (already done, `enums.f:3-4`).
- Docs alignment (`docs/forth.md:295-310`, `docs/type-families.md` enum sections).

**Waits on 6-13:** the block `ENUM color … ;ENUM` defining word,
`COLOR:*` constructors, `MATCH color` exhaustiveness, bad-tag death, and every
negative fixture that needs a real declared enum (spec §25.2/§25.4, §24 codes).

---

## 5. Trust surface — zero new trust rows achievable

- **`enums.f` has NO source `TRUST` site** (whole file read; `enums.f:1-11`
  contains no `TRUST`/`TRUSTED:`/`set-check`). Its `create , … does>`
  (`enums.f:7,10`) is the CREATE/DOES> definer category named in `TRUSTED.md`
  ("Trust only what cannot be inferred — … `CREATE`/`DOES>`…") — but the child
  effect `( -- n )` is inferred/runtime-stamped by the checker's DOES> hook (see
  `docs/census-tfam-8.md` §1c), **not a `TRUSTED.md` manifest row**. So retiring
  or keeping `enums.f` adds/removes **no** manifest trust row.
- **Block enum constructors must publish via item 8's zero-trust checked path.**
  Spec §12 `docs/type-families.md:779-781`: "The generated constructor body is
  checked code. The generator must not emit `TRUST`, `TRUSTED:`, `set-check`, or
  require a `TRUSTED.md` manifest row." A zero-payload `COLOR:RED` body is just
  `<tag> ;` — a checked `:` body of one literal push, the easiest zero-trust case.
- **Gate-17n ratchet (`PLAN.md:989-992`):** "The type-family/ADT campaign may not
  add `TRUST`, `TRUSTED:`, `set-check`, or `TRUSTED.md` rows." Enum families,
  being width-1 zero-payload, are the LEAST likely to need trust.

**Verdict: zero new trust rows is achievable for item 14** (both halves). No gap.
The `type-family.f` / `enums.f` sources carry no trust rows and none is required.

---

## 6. PLAN item 14 acceptance as a checklist (files/words per criterion)

From `PLAN.md:899-902`. Paths declared (`PLAN.md:894-895`): `src/core/enums.f`,
`test/gate-dictionary-lib.f`, `docs/type-families.md`.

1. **"existing enum fixtures either pass through the deliberate legacy spelling
   or are migrated"** → `test/gate-dictionary-lib.f:888-916` `GD-ENUMS` must
   either stay green on `ENUM+`/`ENUM4+` (legacy kept, `src/core/enums.f:6,9`
   unchanged) OR be migrated to block enums. Decision = §3a.
2. **"block-style `ENUM color … ;ENUM` defines checked constructors"** → new
   `ENUM`/`;ENUM` defining words (owner: new `src/core/enum-family.f` or
   `src/core/enums.f`) driving `TFAM-DECL … TK-ENUM` (`type-family.f:293,18`),
   `SUMV-ADD … paycells=0` (`:375`), `TFAM-SLOTS! 0` (`:225`),
   `TFAM-VAR-RANGE!` (`:226`); constructors via item-8 generator (`COLOR:RED`,
   spec §12).
3. **"and exhaustive `MATCH`"** → item-9 `MATCH` accepting `kind = enum`
   (spec §14 `docs/type-families.md:827`); fixture `COLOR>CODE`
   (spec §23 `:1428-1439`).
4. **"duplicate … enum variants reject"** → `SUMV-ADD` `E-TFAM-DUP`
   (`type-family.f:378`); diagnostic "bad enum declaration: duplicate variant
   red" (spec §24 `:1463`, item-13 text).
5. **"missing … enum variants reject"** → non-exhaustive `MATCH` reject (item 9,
   `PLAN.md:676-678`); spec §24 "missing variant err" (`:1468`).
6. **"bad enum variants reject"** → unknown/wrong-family variant reject (item 9);
   `n`↔`color` reject (spec §23 `BAD-COLOR` `:1444-1449`, needs item-7 hidden
   field).
7. **Goal mapping "without silently changing legacy semantics"**
   (`PLAN.md:907-908`) → contiguous 0-based block tags must not be substituted for
   `ENUM4+` stride-4 / non-zero-start numeric families (§3c); those stay `ENUM+`.

### Gate 17n checklist (item 17 applied to item 14, `PLAN.md:958-1027`)
- TDD red fixtures before impl; rebuild `bin/hb`; native self-refresh/fixpoint
  byte-identical; no-binary Gforth bootstrap reaches fixpoint (`:968-970,995-997`).
- **Trust ratchet unchanged** (§5): exact `TRUSTED.md` count + inventory
  before/after item 14 identical (`:989-992`).
- `tools/filemap-lint.f` covers `docs/type-families.md` (`:999-1000`; already
  registered `FILEMAP.md:22`); any new `src/core/enum-family.f` must be added to
  `FILEMAP.md`, `tools/srclist.f`, `test/run-files.f` result-cache keys
  (`:971-972`, cf. `run-files.f:99`), `tools/build-fixpoint.f`
  (`BF-APPEND-*`, cf. `:636-651`), `tools/hb-build-lib.f` key list, and the
  native + Gforth prefix rows (habu2.f / `bootstrap/cg/forth.fs` `LPENUMS`
  neighborhood).
- Stdin-manifest reconciliation across `build-fixpoint.f` / `bootstrap.sh` /
  `srclist.f` / `hb-build-lib.f` / `run-files.f` (`:973-978`).
- Candidate binary size vs `test/gate-build-size.f` (`:993,997-999`); host/filemap
  lints; master advances only on exact-tree green (`:1020-1021`).

---

## 7. Open risks / unknowns — each with a probe

1. **R1 — All dependencies (6-13) unbuilt (§0).** Item 14's block half cannot
   land first. *Probe:* `rg -l '^: SUMTYPE|^: ENUM |PUSH-LOGICAL' src/` (0 today);
   the block `ENUM` probe returns `E-UNDEFINED: ENUM` exit 70. Sequence the block
   half after 6-13; land only the legacy-retire half early if desired.
2. **R2 — Retire vs coexist decision (§3a) not fixed by PLAN.** PLAN says
   "replace OR retire … If compatibility is needed, move behind a legacy name"
   (`PLAN.md:896-898`) — but `ENUM+` is ALREADY the plussed legacy name, so the
   "move behind a legacy name" step is a no-op. *Probe:* `rg -rn '\bENUM4?\+' .`
   → only `enums.f` + `gate-dictionary-lib.f` + docs; with zero prod users,
   coexist is the low-risk default. Decision owner: implementer/orchestrator.
3. **R3 — Enum inherits item-7 kind-blindness (§2c probe).** A `TK-ENUM`
   signature parses one-cell and `drop`/`n`-coerce type-checks TODAY. *Probe:* the
   §2c `mycolor` snippet must flip exit 0 → reject once item 7 lands; re-run
   identically as the item-14 enum regression.
4. **R4 — Tag-0 truthiness (`red` = tag 0).** `?dup`/`if`/`case`/`0=` on an enum
   must reject (raw top-cell truth is unsound, `PLAN.md:813-814`,
   `docs/type-families.md:1303`). *Probe:* after item 12, `: T ( color -- ) if
   … then ;` must reject; before item 12, `MATCH` is the only eliminator.
5. **R5 — `ENUM4+` stride / non-zero start have no block analog (§3c).** Block
   tags are always 0-based contiguous. *Probe:* compare `gate-dictionary-lib.f:900`
   (`100 ENUM+ … ENUM4+ …`) numeric outputs (`:906-913`) against any proposed
   block migration — they differ, so those families must NOT be block-migrated.
6. **R6 — Build-closure drift on retire (§1d).** Retiring `enums.f` touches 7+
   wiring sites. *Probe:* `rg -rn 'enums\.f|LPENUMS|BF-APP;ENUMS' src/ tools/
   bootstrap/ test/` — every hit must be edited in the same commit or native and
   Gforth prefixes diverge (gate 17n stdin-manifest, `PLAN.md:973-978`).

---

## Contradictions (PLAN / spec vs code)

- **C1 — Dependency chain unbuilt; tree is far behind item 14.** Item 14
  `Depends on: items 9-13` (`PLAN.md:906`) and order `13 -> 17m -> 14`
  (`PLAN.md:1031`), but NO ADT grammar/`MATCH`/constructor source exists (§0). The
  block half of item 14 is not buildable in this tree; only the legacy-`ENUM+`
  half is. Not a plan defect — a scheduling reality; do not implement the block
  form before 6-13 land.
- **C2 — Item 14 Paths omit `docs/forth.md` and any new enum-family file.** Paths
  = `src/core/enums.f, test/gate-dictionary-lib.f, docs/type-families.md`
  (`PLAN.md:894-895`). But `docs/forth.md:295-310` documents `ENUM+`/`ENUM4+` and
  the reserved bare `ENUM`, and MUST be updated on any retire/rename or block-ENUM
  introduction. Additionally, Package Shape (`PLAN.md:124-131`, "one concern per
  file", `CLAUDE.md`) favors a NEW `src/core/enum-family.f` for the block grammar
  rather than overloading the counter-definer `enums.f` — a file outside the Paths
  list. Recommend adding `docs/forth.md` (and any new source file) to item 14 Paths.
- **C3 — "the current numeric `ENUM`/`ENUM4` chain" (`PLAN.md:896`) names words
  that do not exist as `ENUM`/`ENUM4`.** The actual definers are `ENUM+` and
  `ENUM4+` (`enums.f:6,9`); bare `ENUM`/`ENUM4` are undefined (§0 probe). The PLAN
  prose predates the `+`-suffix naming split (which already reserves bare `ENUM`,
  `enums.f:3-4`). Code is authoritative: the migration target is `ENUM+`/`ENUM4+`,
  and bare `ENUM` is already free — so PLAN's Risk "reusing `ENUM` without a
  transition will break existing dictionary tests" (`:903-904`) is already
  mitigated (nothing binds bare `ENUM`).
- **C4 — "legacy spelling" vs already-plussed names.** PLAN Acceptance
  (`PLAN.md:899-900`) offers "pass through the deliberate legacy spelling" as an
  option, implying a rename step is pending; but `enums.f` already IS the
  deliberate legacy spelling (`ENUM+`/`ENUM4+`). So the "pass through" option is
  satisfiable with zero code change to `enums.f` — the only work is adding the
  block `ENUM` word and NOT colliding (which it won't, §0). Consistent, but the
  PLAN wording reads as if the rename is still to be done.
- **C5 — Spec §26 has no explicit "enum-family phase" heading** yet §26 Phase-3
  note (`docs/type-families.md:1712-1713`) forward-references "the later
  enum-family phase". The §26 phase list (`:1650-1790`) jumps Phase-3 `SUMTYPE`
  (`:1687`) → Phase-4 stack ops → … → Phase-7 products, with **no dedicated enum
  phase**. Enum is realized as `SUMTYPE _ 0` sugar (§9.3), so it rides Phase-3/8
  infrastructure — but the "later enum-family phase" the note promises is
  unlabeled. Item 14 IS that phase; the spec should either add a §26 enum phase or
  state enum is folded into Phase-3.
