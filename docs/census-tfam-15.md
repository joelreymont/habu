# Item 15 Census — Product Families + VALUE-RECORD/FIELD Migration

Dot: `habu-tfam-15-product-3005cd62`. PLAN.md item 15 (`PLAN.md:910-932`), spec
`docs/type-families.md` §9.4 (`539-551`), §6-8 (`280-447`), §10-11 (`554-641`),
§26 Phase 7 (`1767-1777`), WIDTH `1125`; per-item gate 17o (item 17,
`PLAN.md:958-1027`; order `... 14 -> 17n -> 15 -> 17o -> 16 -> 17p`,
`PLAN.md:1031`). Prose companion `docs/effects.md:126-132`.

Every claim is `file:line` + a quoted definition/snippet. Symbols are
authoritative. Paths relative here for density; absolute in the summary. Repo
root `/Users/joel/Work/habu`.

Term encoding (checker.f value-record node tags): `VR-CON 0 VR-VAR 1 VR-ROW 2
VR-PTR 3 VR-PUSH 4 VR-QUOT 5 VR-ATOM 6 VR-PARAM 7` (`checker.f:1249-1256`).

---

## 0. State of the world (what item 15 builds ON / against)

- **No `PRODUCT`/`FIELD`/`END-PRODUCT` grammar exists.** Probe (read-only,
  `/private/tmp`): `printf ': P1 ( -- ) ;\nPRODUCT\n' | bin/hb --load …` →
  `E-UNDEFINED: PRODUCT`. `rg '^: PRODUCT|END-PRODUCT|: FIELD\b'` over
  `src/ lib/ tools/` = 0 defining words. PLAN reserves the tokens *at this item*:
  "item 15 reserves `PRODUCT`/`FIELD`/`END-PRODUCT`" (`PLAN.md:437`).
- **Prerequisites 6/7/8/12 are UNBUILT** (confirmed by `docs/census-tfam-7.md` §0
  and `docs/census-tfam-8.md` §0; tree is at TFAM item 4). No
  `SUMTYPE`/`TYPEFAMILY` grammar, no `PUSH-LOGICAL`/`LAYOUT-PUSH-FIELDS`, no
  layout-aware `dup`/`drop`. Item 15 `Depends on: item 12` (`PLAN.md:931`) and is
  last-in-line before 16.
- **VREC is the ONLY working by-value record mechanism today** and it is a
  *parallel* registry to TFAM, predating it. Its field cells are **touchable by
  ordinary primitives** (this is the crux of §3/§C4). Proof — a value-record
  `dup` rejects with the field expansion leaked verbatim:
  ```
  $ printf 'VALUE-RECORD point x n y n END-VALUE-RECORD\n: PDUP ( point -- point point ) dup ;\n' | bin/hb --load …
  habu: in pdup: at 'dup' expected: field<point,x,n> field<point,y,n> field<point,x,n> field<point,y,n>
                            actual: field<point,x,n> field<point,y,n> field<point,y,n>
  ```
  `point` = two ordinary cells `field<point,x,n> field<point,y,n>`; `dup`
  duplicates ONE cell → 3 cells → reject. Ordinary `over over`/`drop`/`nip`
  freely destructure it (§1i fixtures).
- **`TK-PRODUCT` + `PF-*` product-field registry EXISTS but has ZERO real
  declaration sites.** `type-family.f:16` `1 constant TK-PRODUCT`; the PF-REC
  registry (`type-family.f:392-447`) and `TFAM-FLD-RANGE!` (`:228`) are
  **exercised only by `test/type-family-suite.f`** via friend-only `TFAM-DECL` /
  `PF-ADD` (`type-family-suite.f:76,215-222,244`). No grammar populates them.
  Item 15 is the item that first drives real products through this substrate.

So item 15 = (a) a `PRODUCT`/`FIELD`/`END-PRODUCT` grammar over the *existing*
TK-PRODUCT/PF registry; (b) a decision on whether `VALUE-RECORD` folds into it or
stays a compat layer; (c) migrating the two real VREC producers (PTX IR + engine
records) and renaming pre-existing `FIELD*` words. The registry substrate is
done; the grammar, the destructure semantics, and the fixture-preservation
constraint are the work.

---

## 1. COMPLETE inventory of today's VALUE-RECORD / FIELD mechanism

### 1a. VREC arenas + registry (checker.f)
Boot buffers + P-pointer indirection (grows relocate; offsets/ids survive):
- Caps `checker.f:1258-1269` (`VREC-CAP-INIT 64`, `VREC-FIELD-INIT 512`,
  `VREC-NODE-INIT $4000`, `VREC-STR-INIT $10000`).
- Boot stores + pointers `checker.f:1273-1319` (`VREC-NAME-A/U`, `VREC-START`,
  `VREC-COUNT`, `VREC-TVN`, `VREC-RVN`, `VREC-FIELDS`, node columns
  `VRN-TAG/A..H`, `VREC-STR`); rebind via `VREC-ARENA-BOOT` (`:1297-1306`).
- Counters `checker.f:1328-1340`: `VREC-N` (records), `VREC-FIELD-N`
  (field-index pool), `VREC-NODE-N` (type-graph nodes), `VREC-STR-U` (string
  bytes), scan indices `VREC-I/J`.
- Grow/ensure `checker.f:1345-1372`; string pool grow+rebase
  `VREC-STR-GROW/REBASE` (`:1453-1468`); string copy `VREC-STR-COPY` (`:1469`).
- Record queries `checker.f:1384-1421`: `VREC-NAME$` (`:1392`), `VREC-START@`
  (`:1396`), `VREC-COUNT@` (`:1400`), `VREC-TVN@`/`VREC-RVN@` (`:1404-1410`),
  `VREC-FIND ( ptr u8 n -- n bool )` (`:1415`) — **flat linear scan, NO package
  key** (contrast TFAM `TFAM-FIND-IN`, `type-family.f:246`).

### 1b. The stored-signature term (VR-* node arena + copy/instantiate)
A value record stores each field's *checker type graph* persistently and
re-instantiates it (fresh vars) per reference — the "stored signatures" surface:
- Node slots `VN.TAG@ … VN.H@` / `VN.TAG! … VN.H!` (`checker.f:1431-1448`) over
  8 columns `VRN-A..H` (`:1281-1288`); `VREC-NODE-NEW` (`:1480`).
- **`VREC-COPY ( n -- n )`** (`checker.f:1532-1590`): serialize a resolved term
  into VR-* nodes. The `VR-PARAM` arm (`:1580-1587`) stores
  `PARAM>NAME-A/U`→string, `PARAM>ARGC`→`VN.C`, **`PARAM>FAM`→`VN.H`** (`:1583`,
  "resolved family-id (identity)"), args0-3→`VN.D..G`. So a `field<>` wrapper is
  stored as a 3-arg VR-PARAM carrying `FIELD-FAM` in `VN.H`.
- **`VREC-INST ( n -- n )`** (`checker.f:1639-1665`): rebuild a fresh checker
  term; the param arm (`:1662`) `node VREC-I-STR node VN.H@ MK-PARAM` re-mints
  with fresh TV/RV maps (`VREC-I-TV/RV`, `:1612-1620`). This is the "VNARG"
  reconstruction the task references (per-arg `VN.D..G`).
- **`VREC-PUSH-FIELDS ( row id -- row )`** (`checker.f:1667-1675`): the
  expansion that makes a record name in a signature become N cells —
  `id VREC-START@ VREC-I @ + VREC-FIELD@ VREC-INST swap MK-PUSH` per field. This
  is the value-record analogue of item 7's `LAYOUT-PUSH-FIELDS`.

### 1c. The `field` wrapper family + `FIELD-FAM`
- Declared PRIVATE, kind `TK-CELL`, arity 3, in reserved package `"@"`:
  `type-family.f:648` `s" @" CHECKER-PACKAGE-PRIVATE s" field" 3 TK-CELL
  TFAM-DECL FIELD-FAM !`. Name is `field` (hidden by *package privacy*, not by
  the `@`-name rule). Because it is `TK-CELL`, `TFAM-LAYOUT?` is FALSE for it
  (`type-family.f:214,218`) — so every `field<>` cell is an **ordinary
  touchable cell**, not an item-7 hidden field.
- `FIELD-FAM` cell `checker.f:359` `variable FIELD-FAM  -1 FIELD-FAM !`; reset by
  `TFAM-RESET` (`type-family.f:511` `-1 FIELD-FAM !`).

### 1d. FIELD-INNER + the field unify arms (checker.f:535-569)
- `FIELD-PARAM? ( t -- bool )` (`:535-538`) — identity by reserved family-id:
  `t T-RES PARAM>FAM FIELD-FAM @ =`.
- `FIELD-REC/NAME/INNER = 0/1/2 PARAM>ARG` (`:540-547`).
- `FIELD-ID-SAME? ( a b -- bool )` (`:555-557`) compares the rec+name **atom
  strings** (`FIELD-ATOM-SAME?`, `:549-553`) — **VREC field identity is
  name/string-based, NOT family-id-based** (contrast a real product, which is
  `family-id`-keyed).
- `FIELD-PAIR? ( got want -- bool )` (`:559-563`), `FIELD-COERCE? ( got want --
  bool )` (`:565-569`): under `UK-COERCE`, a `field<>` **coerces to its inner
  type** (`got FIELD-INNER want PAIR`, `:567`). This is exactly the by-value
  destructure — `field<point,x,n>` coerces to `n` on output — and is precisely
  the coercion item 12 must **reject** for hidden fields (`PLAN.md:783`
  "final signature coercions such as `FIELD-COERCE?` must reject them").
- Dispatched inside `U-TYPE` (`checker.f:911-912`, per census-7 §2e);
  `LIN-TYPE-COUNT*` descends field inner (`checker.f:1074`).

### 1e. VREC-FIELD-WRAP — how a parsed field becomes a `field<>` term
`checker.f:2094-2100`:
```
: VREC-FIELD-WRAP ( ptr u8 n ptr u8 n n -- n ) …
   rec recu MK-ATOM PARAM-SCR+   fld fldu MK-ATOM PARAM-SCR+   typ PARAM-SCR+
   base s" field" FIELD-FAM @ MK-PARAM ;   \ -> field<rec,name,inner>
```
`VREC-FIELD-STORE` (`:2102-2104`) wraps + `VREC-COPY` + `VREC-FIELD!`.

### 1f. Declaration grammar `VALUE-RECORD` (interpret-mode definer)
- `roles.f:172-188` `: VALUE-RECORD ( -- )` — `parse-name` the record name, then
  loop `parse-name` tokens into `VRDEF-BUF` (`roles.f:123-150`) until
  `END-VALUE-RECORD` (`VRDEF-END?`, `:169`), then
  `name nameu VRDEF-BUF VRDEF-U @ CHECKER-DEFRECORD` (`:184`). Grammar is
  `VALUE-RECORD name f1 t1 f2 t2 … END-VALUE-RECORD`; **no `FIELD` keyword,
  arity 0, concrete field types.**
- `CHECKER-DEFRECORD ( name nameu fields fieldsu -- )` `checker.f:2141-2146` →
  `TYPE-RESERVED?` guard, `VREC-BEGIN` (`:2074`), `VREC-PARSE-FIELDS` (`:2126`),
  `VREC-FINISH` (`:2087`). `VREC-PARSE-FIELDS` tokenizes `name type` pairs,
  rejects delimiter/empty/dup (`VREC-FIELD-BAD?` `:2122`, `VREC-FIELD-DUP?`
  `:2114`), parses each type via `SIG-TYPE` (`:2136`), stores via
  `VREC-FIELD-STORE`.

### 1g. Two OTHER independent VALUE-RECORD parsers (must gain PRODUCT too)
All route to the same `CHECKER-DEFRECORD` (3 callers tree-wide: roles.f,
verify-source.f, check-core.f):
- **Source preverify:** `verify-source.f:355-371` `RECORD-VALUE-RECORD` →
  `CHECKER-DEFRECORD` (`:367`); dispatched in `RECORD-DEFINER?`
  (`:373-387`, `value-record` arm `:380`).
- **Check tool nominal scanner:** `tools/check-core.f:677-711` (`CHK-VREC-*`,
  `CHK-VREC-DO-DEF` → `CHECKER-DEFRECORD` `:690`); dispatched in `CHK-NOM-STEP`
  (`:726-739`, `VALUE-RECORD` arm `:736`).
- **No Gforth/bootstrap mirror** of value records exists (`rg
  'value-record|defrecord|vrec' bootstrap/` = 0). A parametric PRODUCT with
  cross-engine constructor naming (item 8/10 territory) would change that.

### 1h. Snapshot persistence + transactional rollback (unchanged surface)
- Core rollback frame saves the VREC high-water marks:
  `RBF.VRECN/VRECF/VRECND/VRECU` (`checker.f:5153-5156`); saved `RBF-PUSH`
  (`:5205-5208`), restored `RBF-POP` (`:5230-5233`). Spec `type-families.md:1249`
  lists `VREC-N VREC-FIELD-N VREC-NODE-N VREC-STR-U` in the core frame.
- Snapshot persist `VREC-SNAPSHOT-PERSIST` (`checker.f:3940-3961`) bakes every
  VREC store into image DATA (rebases `VREC-STR`, `:3959-3961`). Called from
  `CHECKER-SNAPSHOT-PREPARE` (`:3984`). A migration to TFAM/PF/SCHEMA persistence
  would instead ride `TFAM-SNAPSHOT-PERSIST` (`type-family.f:594-602`, already
  bakes PF-* `:598`) + `SCHEMA-SNAPSHOT-PERSIST` — **two persistence models to
  reconcile**, not merge blindly.

### 1i. EVERY declaration site + consumer in the tree
**Real declaration sites (5 records, 2 files):**
- `test/engine-suite.f:932-935` — `point` (`x n y n`), `rect` (`w n h n`), `box`
  (`value a`, arity-1 param field), `hdl` (`owner own raw ptr u8`, **linear
  payload** via `own`).
- `lib/ptx/ir.f:18` — `ptxir-node` (`op n a n b n val n live n`, 5-cell record):
  the production migration target named by PLAN (`PLAN.md:911,920-921`).

**Consumers:**
- `test/engine-suite.f:936-965` — point words `T->POINT`/`T-POINT>`/
  `T-POINT-DUP over over`/`T-POINT-X drop`/`T-POINT-Y nip`/`T-POINT-X! swap
  drop`/`T-POINT-Y! >r drop r>` (`:936-942`); runtime `T{`-style tests
  (`:945-951`); checker fixtures `COK-POINT-*`/`COK-BOX-*`/`COK-HDL-PASS`
  (`:952-960`) and `CBAD-POINT-RECT`/`CBAD-POINT-DUP dup`/`CBAD-POINT-PARTIAL`/
  `CBAD-BOX-RECT`/`CBAD-HDL-DUP over over` (`:961-965`).
- `lib/ptx/ir.f:75-135` — `>PTXIR-NODE ( n n n n n -- ptxir-node )` (`:75`),
  `PTXIR-NODE> ( ptxir-node -- n n n n n )` (`:77`), `PTXIR-NODE-DROP
  ( ptxir-node -- ) drop drop drop drop drop` (`:79-80`, **5 raw drops**),
  `PTXIR-NODE-DUP-RAW` (`:82-85`), `PTXIR-NODE-DUP` (`:87-88`), `PTXIR-WRITE`
  (`:98`), `PTXIR-MATCH?` (`:109`), `PTXIR-FIND` (`:118`), `PTXIR-NODE-INTERN`
  (`:124`). NB the physical node store is a *separate* `BEGIN-STRUCTURE
  PTXIR-REC` + `PTXIR-NODES` array (`:20-28`); the value record only types the
  on-stack bundle, so migration does not touch storage.
- `tools/check-test-lib.f:205-259` — string fixtures re-run through the check
  tool: `CKT-VREC-GOOD$` (`:205-236`, point/box/hdl + accessor words),
  `CKT-VREC-BAD$` (`:245-252`), `CKT-VREC-PARTIAL$` (`:254-259`); driver
  `CKT-TEST-VALUE-RECORD-GOOD/BAD/PARTIAL` (`:392-454`).
- `docs/effects.md:126-132` — the normative prose (see §C5).
- **maki: NONE.** `rg -i value-record maki/` = 0 (see §C6).

---

## 2. TK-PRODUCT kind + PF-* registry, and what a product gives that VREC lacks

### 2a. Product family kind + product-field registry (type-family.f)
- `TK-PRODUCT` `type-family.f:16`; predicate `TFAM-PRODUCT?` (`:215`);
  `TFAM-LAYOUT?` includes it (`:218-219`).
- TF-REC product-field range: `TF.FLD-START`/`TF.FLD-COUNT` (`:161-162`),
  readers `TFAM-FLD-START@`/`TFAM-FLD-COUNT@` (`:203-204`), writer
  `TFAM-FLD-RANGE!` (`:228`).
- **PF-REC** (`type-family.f:392-398`): `PF.FAM PF.NAME-OFF PF.NAME-U PF.SCH
  PF.SLOT`. Ops: `PF-ADD ( fam name-a name-u sch slot -- id )` (`:437-447`,
  canon-checks + dup-rejects), `PF-FIND` (`:430`), `PF-FAM@`/`PF-NAME$`/`PF-SCH@`/
  `PF-SLOT@` (`:420-424`). **Keyed by (family-id, field tail)** — package-scoped
  and id-based, unlike VREC's flat name-keyed store.
- Layout record `LAY-*` (`:452-503`) for physical size/align/tagw (item 16).
- Rollback + persist already wire PF/LAY (`TFRB.PFN` `:530`, `PF-N @`
  `:557,567`; persist `:598`).
- **Proof the substrate works** — `test/type-family-suite.f`: `s" pkgc"
  CHECKER-PACKAGE-PUBLIC s" pt" 0 TK-PRODUCT TFAM-DECL PTID !` (`:76`),
  `TFAM-PRODUCT? -1 T=` `TFAM-LAYOUT? -1 T=` (`:93`), `TFAM-FLD-RANGE!` +
  `TFAM-FLD-START@`/`COUNT@` (`:169`), `PF-ADD`/`PF-FAM@`/`PF-SLOT@`/`PF-FIND`
  (`:215-222`), `LAY-ADD` (`:231`), a 2-arity `pair` product (`:244`).

### 2b. What PRODUCT provides that VREC lacks (per spec)
| Property | VREC (today) | PRODUCT (spec §6-11) | Evidence |
|---|---|---|---|
| Registry key | flat, name-only, global | `(package, tail)` id-keyed | `checker.f:1415` vs `type-family.f:246,430` |
| Parametricity | none (arity 0, concrete field types) | arity N; fields are `paramref` schemas | spec `type-families.md:544-547` (`FIELD fst a`) |
| Field type storage | VR-* type-graph copy per record | persistent SCHEMA nodes (`SC-PARAMREF`/`SC-CON`) | `checker.f:1532` vs spec `:423-437` |
| Identity | field rec+name **atom strings** | `family-id` | `checker.f:555-557` vs spec `:334-337` |
| Field cells | **touchable** ordinary cells (coerce→inner) | item-7 hidden, **untouchable** | `checker.f:565-569` vs spec `:607-610`, `PLAN.md:779-784` |
| Layout policy | stack-cell only | packed/niche/boxed (item 16) | spec `:1779-1790`, `type-family.f:24-28` |
| Logical rendering | leaks `field<point,x,n>` | logical `point` / `@point.slotN` compaction | probe §0, spec `:554-611`, item 13 |
| Width | N field cells | `WIDTH(product<…>) = sum field widths` (no tag) | spec `type-families.md:1125` |

- **Size parity (no regression):** product has **no tag cell**
  (`type-families.md:1125`), so `ptxir-node`→product = 5 cells, `point`→product =
  2 cells — byte-identical to VREC. This satisfies "no size regression"
  (`PLAN.md:921,928`).

### 2c. What "PRODUCT-unified value records and PTX IR" requires (per PLAN/goal)
1. `PRODUCT name arity … FIELD f τ … END-PRODUCT` grammar populating
   `TFAM(TK-PRODUCT)` + `PF-ADD` rows + SCHEMA field schemas, reserving
   `PRODUCT`/`FIELD`/`END-PRODUCT` (`PLAN.md:437`, `type-families.md:539-551`).
   Grammar needed in **all three** parsers (§1f-g) + any Gforth mirror.
2. By-value construction/destructure proven **without size regression**
   (`PLAN.md:920-921`) — for `ptxir-node` this is `>PTXIR-NODE`/`PTXIR-NODE>` and
   the DUP/DROP path; needs item-12 layout-aware `dup`/`drop`/`over`/`nip`.
3. The decision (`PLAN.md:918-920`): `VALUE-RECORD` → product sugar **or** typed
   compat layer "over the same registry". §3/§C4 show the fixture-preservation
   clause forces "compat layer" unless the fixtures are rewritten.

---

## 3. Migration surface (per site) + what could break

### 3a. THE central break: touchable field cells vs layout-aware/hidden fields
VREC destructures a record with **ordinary** `drop`/`nip`/`over over`/`swap
drop`, relying on (i) field cells being touchable and (ii) `FIELD-COERCE?`
collapsing a field to its inner. Item 7 makes hidden fields **untouchable** and
item 12 makes `dup dup drop swap over nip …` **whole-bundle** and makes
`FIELD-COERCE?` **reject** hidden fields (`PLAN.md:765,779-784`). Therefore a
straight `VALUE-RECORD → PRODUCT` migration **flips existing fixtures**:

| Fixture (today) | Today | After product+item12 | File |
|---|---|---|---|
| `T-POINT-X ( point -- n ) drop` | PASS | REJECT (drop = whole bundle; no coerce) | `engine-suite.f:939,955` |
| `T-POINT-Y ( point -- n ) nip` | PASS | REJECT | `:940,956` |
| `T-POINT-X! ( n point -- point ) swap drop` | PASS | REJECT | `:941,957` |
| `CBAD-POINT-DUP ( point -- point point ) dup` | REJECT | PASS (dup = whole bundle) | `:962` |
| `COK-POINT-DUP ( point -- point point ) over over` | PASS | REJECT/ambiguous (bundle over) | `:953` |
| `PTXIR-NODE-DROP ( ptxir-node -- ) drop drop drop drop drop` | PASS | REJECT (4 underflow drops) | `lib/ptx/ir.f:79-80` |
| `PTXIR-NODE-DUP-RAW` (destructure+rebuild) | PASS | rewrite to 1 layout `dup` | `lib/ptx/ir.f:82-88` |

**Consequence:** "existing value-record fixtures pass" (`PLAN.md:922`) is only
achievable if (a) `VALUE-RECORD` remains a distinct compat layer whose `field<>`
cells stay `TK-CELL`/touchable (they never become `TFAM-LAYOUT?`, so item 12
never rewrites them), **or** (b) the fixtures are migrated to
constructor/accessor/`MATCH` style and the flips are accepted as intended new
semantics. The migrated `ptxir-node` accessors (`PTXIR-NODE-DROP`,
`PTXIR-NODE-DUP-RAW`) MUST be rewritten regardless if it becomes a product.

### 3b. Stored signatures (VR-* / VNARG) → SCHEMA
Each VREC field is a VR-PARAM node carrying `FIELD-FAM` in `VN.H`
(`checker.f:1583`), re-instantiated by `VREC-INST`/`MK-PARAM` (`:1662`) with
fresh vars. A product stores fields as persistent SCHEMA nodes (`SC-CON` for
concrete `n`/`ptr u8`; `SC-PARAMREF` for a param field like `box`'s `a`), spec
`type-families.md:423-447`. Migration must move each field type from the VR-*
copy into a SCHEMA root (`TF.SCHEMA-ROOT`, `type-family.f:164`) and expand via
`LAYOUT-PUSH-FIELDS` (item 7) instead of `VREC-PUSH-FIELDS`. **Breakage risk:**
the two `box`/`hdl` fields are non-trivial — `box value a` is a **parametric**
field (`a`), `hdl owner own` is a **linear** field (`own`) — both must survive
the SCHEMA round-trip (parametricity is new for VREC; linearity is item 11).

### 3c. Snapshot persistence (item 15 must not desync image DATA)
If records leave the VREC arenas, `VREC-SNAPSHOT-PERSIST` (`checker.f:3940`) and
the `RBF.VREC*` frame cells (`:5153-5156`) either keep serving a shrinking compat
population or must be retired; the product half already persists via
`TFAM-SNAPSHOT-PERSIST` (`type-family.f:594-602`) + SCHEMA. Gate 17o result-cache
ABI/source keys (`PLAN.md:970-978`) must be re-keyed if a record moves registries.

### 3d. Hidden fields from item 7 (rendering) + layout-aware ops from item 12
- Diagnostics **already leak** internal `field<point,x,n>` names (probe §0). A
  product renders logical `point` or hidden `@point.slotN` — the "logical
  rendering" acceptance (`PLAN.md:923`) is owned by the renderer `PARAM-START`
  (`render.f:102`), `QREND` T-PARAM arm (`:155-158`), `REND-TYPE` (`:169`), and
  needs item-13 compaction to not regress into `@point.slotN` leakage.
- Item 12 layout-aware primitives (`PLAN.md:760-817`) are the hard prerequisite:
  by-value destructure of a product `ptxir-node` cannot be sound until every
  `dup`/`drop`/`swap`/`over`/`nip`/`rot`/`2*` variant and native/JIT/Gforth
  shuffle is width-aware. This is why `Depends on: item 12` (`PLAN.md:931`).

### 3e. Reserved-name `FIELD` migration
Reserving `FIELD` (and `PRODUCT`/`END-PRODUCT`). Pre-existing `FIELD*` words to
rename (`PLAN.md:914-916`): `lib/object.f` `FIELD-CAP` (`:15`), `FIELD-BYTE`
(`:90`), `FIELD+` (`:97`), `HASH-FIELD` (`:111`), `HEX-FIELD` (`:119`), `FIELD$`
(`:226`), plus `OBJ:ROW-FIELD#`/`OBJ:ROW-FIELD$`; `lib/object-test.f` `FIELD+`
(`:28`) + call sites (`:34-53`, `:117-132`). **None is the bare token `FIELD`**
(§C2). `src/habu/aot-lib.f` has **zero** `field` words (§C1). `src/core/
structures.f` owns `+FIELD` (`:23`), `PTR-FIELD:` (`:27`), `CFIELD:` (`:35`) —
distinct tokens, not renamed by this criterion but the file where a product
`FIELD` block-word could live.

---

## 4. Dependencies: what item 15 consumes from 6/7/8/12; early vs waits

- **Item 6 (grammar/reservation, UNBUILT):** the `SUMTYPE`/`TYPEFAMILY` defining
  machinery + token reservation this item mirrors for `PRODUCT`; item 6 also
  reserves the sibling tokens (`PLAN.md:434`). Item 15 reserves
  `PRODUCT`/`FIELD`/`END-PRODUCT` itself (`:437`).
- **Item 7 (hidden fields / `PUSH-LOGICAL` / `LAYOUT-PUSH-FIELDS`, UNBUILT):**
  the layout expansion + hidden-field kind + fail-closed binding a product's
  fields ride (`PLAN.md:549-556`). Item 15's product fields ARE the "product"
  half of `TFAM-LAYOUT?` (`type-family.f:218`).
- **Item 8 (generated constructors, UNBUILT):** the trust-free `E-ADD-EFFECT`
  publish path for a product's constructor/accessors, and the SUMV/PF ctor-cell
  fill (`census-tfam-8.md` §3b). A product needs generated `PKG:pair`-style
  construct + field accessors instead of empty-body identity casts.
- **Item 12 (layout-aware stack ops, UNBUILT):** the hard blocker for by-value
  destructure (§3a/§3d), named as the sole `Depends on` (`PLAN.md:931`).

**Migratable early vs waits:**
- **Early (independent of ADT machinery):** the `FIELD*` renames in
  `lib/object.f`/`lib/object-test.f` (§3e) + their tests — pure alpha-rename, no
  checker change. **Do first** (`PLAN.md:914-916` "before reserving `FIELD`").
- **Registry substrate:** DONE (`type-family.f` TK-PRODUCT + PF, §2a).
- **Waits on 6/7/12:** the grammar, the by-value destructure of `ptxir-node`/
  engine records, and the fixture decision. Item 15 genuinely sits last before
  16 (`PLAN.md:1031`). Do not migrate `ptxir-node` off VREC until item 12 lands,
  else `PTXIR-NODE-DROP`/`-DUP` break with no sound replacement.

---

## 5. Trust surface — zero new trust rows achievable?

**Yes, zero new trust rows.** Evidence over item-15 touched files:
- `lib/object.f` / `lib/object-test.f`: `rg 'TRUST|set-check'` = **0**. Renames
  add no trust.
- `test/engine-suite.f`: many `TRUST` rows (`:294-548`) but all pre-existing PTX/
  role/image fixtures, none about records; a product migration reuses the
  structured `CHECKER-DEFRECORD` path (no `TRUST`).
- `src/habu/aot-lib.f`: has `0 set-check` (`:15`) + `AOT-PB@ … TRUST` (`:22`) —
  but **no field words** (§C1), so these are unrelated to item 15 and must not be
  extended.
- `src/core/roles.f`: the only checked-record producer, `VALUE-RECORD` →
  `CHECKER-DEFRECORD` (`roles.f:184`), adds **no** trust today (contrast
  `DEFTYPE`/`DTC-EVAL`, `roles.f:32`, which DOES). A product grammar must copy
  the record path, **not** the `evaluate`-based deftype path.

**Precise gap to avoid:** if product constructors/accessors are materialized via
`evaluate` (the `DTC-EVAL` shape, `roles.f:32`, `TRUSTED.md:281`), that adds a
trust row and violates the ratchet (`PLAN.md:989-992`). The sound path is
structured `E-ADD-EFFECT` / checked `:` (per `census-tfam-8.md` §6). No new
`TRUSTED.md` row is required for item 15.

---

## 6. PLAN item 15 acceptance as a checklist (files/words per criterion)

From `PLAN.md:922-927`:

1. **"existing value-record fixtures pass"** → `test/engine-suite.f:932-965`,
   `tools/check-test-lib.f:205-259`. **BLOCKING tension (§3a/§C4):** passes only
   if `VALUE-RECORD` stays touchable-compat OR fixtures are migrated.
2. **"by-value construction/destructure"** → new `PRODUCT` grammar (roles.f or
   new `src/core/product.f`, cf. `src/core/enums.f`) + item-8 constructors +
   item-12 layout ops; proven on `lib/ptx/ir.f:75-135` and a `pair`-style
   product. No size regression: `WIDTH(product)=Σfields` (`type-families.md:1125`).
3. **"hidden fields"** → item-7 `LAYOUT-PUSH-FIELDS` for `TFAM-PRODUCT?`
   (`type-family.f:215`); `@product.slotN` reject in the public parser
   (`TF-HIDDEN?`, `type-family.f:96-98`).
4. **"logical rendering"** → `render.f` `PARAM-START`(`:102`)/`QREND`(`:155`)/
   `REND-TYPE`(`:169`); must render logical `point`, not `field<…>` (probe §0
   leak) or `@point.slotN` (item-13 compaction).
5. **"package visibility"** → `TFAM-VIS@`/`TFAM-PUBLIC?` (`type-family.f:196,213`)
   + `PF-FIND` id-keying; two same-tail products in different packages disjoint
   (already tested `type-family-suite.f:244`).
6. **"linear payloads"** → `hdl owner own` (`engine-suite.f:935`);
   `LIN-TYPE-COUNT*` field descent (`checker.f:1074`); `CBAD-HDL-DUP over over`
   reject (`:965`); gated on item 11 for the general product-linear case.
7. **"reserved-name lint proves no pre-existing `FIELD` remains"** → rename
   `lib/object.f`/`lib/object-test.f` `FIELD*` (§3e); **note no bare `FIELD`
   exists today (§C2)** and `aot-lib.f` has none (§C1); renamed helpers keep
   their tests green (`lib/object-test.f`).
8. **"docs distinguish supported vs legacy surfaces"** → `docs/type-families.md`
   §9.4 (`:539-551`) + `docs/effects.md:126-132` (must resolve §C5 self-
   contradiction).

### Gate 17o checklist (item 17 applied to item 15, `PLAN.md:958-1021`)
- TDD red product fixtures before impl; rebuild `bin/hb`; native
  self-refresh/fixpoint (`:968-970,995-997`).
- **Trust ratchet unchanged** (§5, `PLAN.md:989-992`) — no `TRUST`/`TRUSTED:`/
  `set-check`/`TRUSTED.md` rows added.
- `tools/filemap-lint.f` covers `docs/type-families.md` (`:999-1000`) — already
  `FILEMAP.md:22,44`; `docs/effects.md` (`FILEMAP.md`) if newly product-aware.
- Result-cache closure/source-list (`tools/srclist.f`) updated for any **new core
  file** (e.g. `src/core/product.f`) — `enums.f` is the precedent
  (`src/core/enums.f` exists for item 14); `FILEMAP.md`/build-cache keys
  (`PLAN.md:970-978`).
- `GE-CANDIDATE-SIZE-CHECK` vs `test/gate-build-size.f` (`:997-999`); no-binary
  Gforth bootstrap to fixpoint (`:995-996`).
- Master advances only on exact-tree green (`:1020-1021`).

---

## 7. Open risks / unknowns (each with a probe)

- **R1 — Touchable→untouchable flip breaks fixtures (§3a).** *Probe:* the §0
  `PDUP dup` reject must become PASS under a product+item12 `dup`; `T-POINT-X
  drop` must flip PASS→REJECT. Re-run both after item 12; if the decision is
  "compat layer", confirm `point` stays `TFAM-CELL?`-style touchable and item 12
  leaves it untouched.
- **R2 — `PTXIR-NODE-DROP` = 5 raw drops (`lib/ptx/ir.f:79-80`).** *Probe:* after
  item 12, `printf '… PRODUCT ptxir-node … drop drop drop drop drop …'` must
  underflow; rewrite to one layout `drop`. Confirm the physical `PTXIR-NODES`
  array path (`:20-49`) is untouched (storage is separate from the bundle type).
- **R3 — Parametric field `box value a` round-trips through SCHEMA.** VREC stores
  `a` as a fresh VR-VAR; a product needs `SC-PARAMREF`. *Probe:* declare
  `PRODUCT box 1 FIELD value a END-PRODUCT`, check `T->BOX ( a -- box )` /
  `T-BOX> ( box -- a )` (mirror `engine-suite.f:943-944,959`).
- **R4 — Linear product `hdl owner own`.** *Probe:* declare the product form,
  assert `COK-HDL-PASS ( hdl -- hdl )` passes and `over over` rejects; gated on
  item 11 (`PLAN.md:815-817`).
- **R5 — Diagnostic leak of internal field names.** *Probe (done §0):* current
  reject prints `field<point,x,n>`. After product+item7, confirm neither
  `field<…>` nor `@point.slotN` reaches stderr (needs item-13 compaction);
  if it does, land minimal render suppression as an item-13 seam.
- **R6 — Snapshot/cache desync when a record leaves VREC (§3c).** *Probe:* build
  `bin/hb`, migrate one record, run `test/run-files.f` result-cache + native
  fixpoint; a stale `RBF.VREC*` or `VREC-SNAPSHOT-PERSIST` key surfaces as a
  fixpoint diff.
- **R7 — Three parsers + no Gforth mirror (§1f-g).** *Probe:* `rg
  CHECKER-DEFRECORD` (3 callers: roles.f, verify-source.f, check-core.f). A
  `PRODUCT` grammar must land in all three; confirm the check tool
  (`CHK-NOM-STEP`, `check-core.f:726`) and preverify (`RECORD-DEFINER?`,
  `verify-source.f:373`) each grow a `product` arm, else a product file fails
  preverify/check-tool passes.
- **R8 — Decision authority (sugar vs compat).** `PLAN.md:918-920` leaves it to
  evidence. *Probe:* the fixture flip table (§3a) is the evidence — "existing
  fixtures pass" + "no size regression" points to **compat layer with touchable
  cells**, with new PRODUCT for parametric/hidden-field/packed use. Record the
  decision + rationale in `docs/type-families.md` (`type-families.md:550,1777`).

---

## Contradictions (PLAN/spec vs code)

- **C1 — `src/habu/aot-lib.f` has ZERO `FIELD` words.** Item 15 lists it as a
  file whose "pre-existing `FIELD` words … and their call sites" must be renamed
  (`PLAN.md:914-916`), but `rg -i field src/habu/aot-lib.f` = 0. Its only
  audited-boundary lines (`0 set-check` `:15`, `AOT-PB@ TRUST` `:22`) are
  unrelated. The path is a phantom in item 15's reserved-name work.
- **C2 — No bare `FIELD` word exists; the migration targets don't literally
  collide with an exact-token reservation.** The real words are `FIELD+`,
  `FIELD-BYTE`, `HASH-FIELD`, `HEX-FIELD`, `FIELD$`, `FIELD-CAP`,
  `OBJ:ROW-FIELD#/$` (`lib/object.f:15,90,97,111,119,226`; `object-test.f:28`)
  and `+FIELD`/`PTR-FIELD:`/`CFIELD:` (`structures.f:23,27,35`) — all distinct
  from `FIELD`. Case-folded lookup folds `FIELD`↔`field` only, not `FIELD+`↔
  `FIELD`. The rename is either precautionary (against a *prefix/substring*-based
  reserved-name lint) or over-specified; the implementer must confirm the lint's
  matching rule before mass-renaming, and pin whether a global `FIELD` block-word
  actually shadows anything.
- **C3 — Item 15 `Paths` omit the core VREC/PRODUCT files.** Declared paths
  (`PLAN.md:911-913`) are `structures.f roles.f lib/ptx/ir.f engine-suite.f
  lib/object.f lib/object-test.f aot-lib.f docs/effects.md
  docs/type-families.md`. But the grammar's target `CHECKER-DEFRECORD` +
  `VREC-*`/`FIELD-*`/`LAYOUT-PUSH-FIELDS` live in `src/core/checker.f`; the
  registry (`TK-PRODUCT`/`PF-*`) in `src/core/type-family.f`; the other two
  parsers in `src/habu/verify-source.f` and `tools/check-core.f`; the fixtures in
  `tools/check-test-lib.f`; rendering in `src/core/render.f`. All must be edited.
  Expect a new `src/core/product.f` (cf. `src/core/enums.f`).
- **C4 — "existing value-record fixtures pass" conflicts with item-7/12
  semantics.** VREC destructures with touchable `drop`/`nip`/`over over` and
  `FIELD-COERCE?→inner` (`checker.f:565-569`, `engine-suite.f:939-965`); item 12
  makes those whole-bundle and makes `FIELD-COERCE?` **reject** hidden fields
  (`PLAN.md:765,783`). The two clauses are consistent only if `VALUE-RECORD`
  stays a compat layer with touchable `TK-CELL` field cells, OR the fixtures are
  rewritten. Not silently reconcilable — the plan's own "decide by evidence"
  (`:918`) is where this lands.
- **C5 — `docs/effects.md` is internally self-contradictory on value records.**
  `docs/effects.md:127` "The token expands to **hidden field types**" vs `:131`
  "accessors, updaters, copies, and destructors are **normal checked words over
  the expanded stack cells**" — the latter documents touchable cells (the real
  behavior), the former borrows item-7's "hidden" wording. Item 15 must fix this
  doc to distinguish VREC's touchable model from a product's hidden-field model.
- **C6 — "maki value records" do not exist.** The dot task and campaign goal name
  maki value records as PRODUCT-unification targets, but `rg -i
  'value-record|\bfield\b|product' maki/` finds none. The only real by-value
  record producers are `lib/ptx/ir.f` (PTX IR) and `test/engine-suite.f`. The
  maki migration is aspirational/future, not a present code surface.
- **C7 — `Depends on: item 12` understates the chain.** Item 15 also consumes
  item 6 (grammar/reservation), item 7 (hidden fields/`LAYOUT-PUSH-FIELDS`), and
  item 8 (generated constructors/accessors). These precede 12 in the order
  (`PLAN.md:1031`) so item 12 transitively implies them, but a reader taking
  "Depends on: item 12" literally would miss that the grammar (6) and hidden-
  field/constructor plumbing (7/8) are equally hard prerequisites.
