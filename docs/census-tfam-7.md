# Item 7 Census — Hidden Physical Fields + Logical Row Expansion (reject-only)

Dot: `habu-tfam-7-hidden-9403ae04`. PLAN.md item 7 (`PLAN.md:546-569`), spec
`docs/type-families.md` §10-11 (§17/§20 forward refs), per-item gate 17g
(`PLAN.md:958-1027`; sequence `... 5 -> 17e -> 6 -> 17f -> 7 -> 17g -> 12 ...`,
`PLAN.md:1031`).

Every claim is `file:line` + a quoted definition/snippet. All paths absolute in
the summary; relative here for density (repo root `/Users/joel/Work/habu`).

Term encoding (from census-tfam-4, re-verified): 3-bit tag `TAG = x and 7`
(`checker.f:116`), `PAY = x >> 3` (`checker.f:118`). Tags:
`0 T-CON 1 T-VAR 2 T-PTR 3 S-ROW 4 S-PUSH 5 T-QUOT 6 T-ATOM 7 T-PARAM`
(`checker.f:1-3`).

---

## 0. State of the world (what item 7 builds ON)

- **Item 2 registries: DONE.** `src/core/type-family.f` (TFAM/SUMV/PF/LAY, 653
  lines) and `src/core/type-schema.f` (SCHEMA nodes) exist and load in the
  checker prefix. Load order (`habu2.f:451-456`):
  `util.f → structures.f → checker.f → type-schema.f → type-family.f → render.f`.
  So **checker.f loads BEFORE type-family.f** — checker.f reaches the registry
  only through forward xt cells installed at type-family.f load.
- **Item 4 (whitelist → TFAM lookup): DONE.** The old `PARAM-CTOR?` whitelist is
  gone; `SIG-FAM?` (`checker.f:1830`) resolves any registered family:
  `: SIG-FAM? ( ptr u8 n -- n bool )  s" " 2swap TFAM-RESOLVE* ;`.
- **Item 6 (`TYPEFAMILY`/`SUMTYPE` grammar): NOT DONE.** Tree-wide search for
  `SUMTYPE`/`TYPEFAMILY`/`END-SUMTYPE`/`VARIANT` defining words returns only the
  spec, PLAN, and one comment line (`checker.f` "waits on the … declaration
  grammar (PLAN item 6)"). The mutators `SUMV-ADD`, `TFAM-SLOTS!`,
  `TFAM-VAR-RANGE!`, `LAY-ADD` are **defined but never called** outside
  `type-family.f`. **Every registered family today is `TK-CELL`**
  (`type-family.f:629-648`). `TK-SUM`/`TK-ENUM`/`TK-PRODUCT` are declared kind
  constants (`type-family.f:15-18`) with **zero instances**. See Contradictions
  §C1: item 7's dependency (item 6, `PLAN.md:568`) is unbuilt.

**Probe — the exact unsoundness item 7 closes.** A hand-registered sum family
used in a checked signature is TODAY accepted as one polymorphic cell (WRONG):

```
$ printf 's" " CHECKER-PACKAGE-PUBLIC s" myresult" 2 TK-SUM TFAM-DECL drop\n: USESUM ( myresult<n,n> -- n ) drop 0 ;\n." ok" cr\n' | bin/hb
ok            # exit 0 — myresult<n,n> parsed as ONE cell, `drop 0` type-checks
```

`SIG-FAM?` resolves `myresult` (kind is never consulted); `SIG-TYPE` builds one
`T-PARAM` and `MK-PUSH`es one cell (`checker.f:2032`). Item 7 must make this
reject (expand to hidden fields internally, fail-closed on the one-cell `drop`).

---

## 1. Where field metadata lives today

### 1a. TFAM record — the family kind + layout + slot fields item 7 reads
`type-family.f:148-167` `BEGIN-STRUCTURE TF-REC`. Relevant fields:
- `TF.KIND` (`:155`) → `TFAM-KIND@` (`:198`); predicates `TFAM-SUM?` (`:216`),
  `TFAM-ENUM?` (`:217`), `TFAM-PRODUCT?` (`:215`), `TFAM-CELL?` (`:214`).
- `TFAM-LAYOUT?` (`:218-219`) — the driver predicate for `PUSH-LOGICAL`:
  `: TFAM-LAYOUT? ( id -- bool ) id TFAM-PRODUCT? id TFAM-SUM? or id TFAM-ENUM? or ;`
- `TF.SLOTS` (`:158`) = max-payload-cells → `TFAM-SLOTS@` (`:200`) /
  `TFAM-SLOTS!` (`:225`). This is `M` in the `M slots + 1 tag` expansion. **Zero
  today** (`0 r TF.SLOTS !`, `:311`); only `TFAM-SLOTS!` (item 6/8) sets it.
- `TF.LAYOUT` (`:157`, policy) `TFAM-LAYOUT-POLICY@` (`:199`); `TF.TAGW` (`:163`,
  default `TAGW-CELL`, `:38`,`:314`) `TFAM-TAGW@` (`:205`); `TF.VAR-START/COUNT`
  (`:159-160`) for variants; `TF.SCHEMA-ROOT` (`:164`).

### 1b. SUMV variant registry (payload widths per variant)
`type-family.f:323-334` `BEGIN-STRUCTURE SUMV-REC`; `SV.PAYCELLS` (`:330`) →
`SUMV-PAYCELLS@` (`:362`), `SV.TAG` (`:327`), `SV.SCH-START/COUNT` (`:328-329`).
`SUMV-ADD` (`:375-387`). **Never populated** (no grammar). Item 7 reject-only
needs `TF.SLOTS` (count of hidden slot fields) but not per-variant schemas
(those are item 8 constructors / item 9 MATCH).

### 1c. Product-field registry `PF-*`
`type-family.f:392-398` `BEGIN-STRUCTURE PF-REC`: `PF.FAM`,`PF.NAME-OFF`,
`PF.NAME-U`,`PF.SCH`,`PF.SLOT`. Ops `PF-ADD` (`:437-447`), `PF-FIND` (`:430-436`),
`PF-FAM@`/`PF-NAME$`/`PF-SCH@`/`PF-SLOT@` (`:420-424`). **Unused** — product
families are item 15, out of item 7 scope. Counter `PF-N` participates in
rollback (`TFRB.PFN`, `:530`,`:557`,`:567`) and snapshot (`:598`). Item 7 does
NOT touch PF-* except by leaving its rollback/snapshot wiring intact.

### 1d. The private `field` family + `FIELD-FAM`
`type-family.f:648`:
`s" @" CHECKER-PACKAGE-PRIVATE s" field" 3 TK-CELL TFAM-DECL FIELD-FAM !`
— registered **PRIVATE, kind `TK-CELL`, arity 3, in reserved package `"@"`** (an
unspellable user package). Its **name is `field`, NOT `@field`** — so it is
hidden by *package privacy*, not by the `@`-name rule (`TF-HIDDEN?`,
`type-family.f:96-98`, tests the leading byte of the *name*). `FIELD-FAM` cell is
declared in checker.f (`checker.f:359`
`variable FIELD-FAM   -1 FIELD-FAM !`) and reset by `TFAM-RESET`
(`type-family.f:511` `-1 FIELD-FAM !`).

Because `field` is `TK-CELL`, **`TFAM-LAYOUT?` is FALSE for it** → `PUSH-LOGICAL`
must keep it one-cell. Value-record fields never flow through `SIG-TYPE` from a
user signature anyway: `PSTACK` catches the VREC name first via `VREC-FIND` and
expands through `VREC-PUSH-FIELDS` (`checker.f:2029-2030`), not `SIG-TYPE`. This
is the "existing `field<...>` behavior preserved" path item 7 must not disturb.

### 1e. `FIELD-INNER` / `T-RES` interaction (checker.f)
- `T-RES ( n -- n )` (`checker.f:482-486`) resolves a term through the var chain
  (compress unless inside a trial). Every `PARAM>ARG`/`FIELD-INNER` access needs
  a **resolved** param term (its payload indexes the PARAM arena).
- `FIELD-PARAM?` (`checker.f:535-538`):
  `FIELD-FAM @ 0 < IF RES-FALSE EXIT THEN  t T-RES TAG T-PARAM <> IF RES-FALSE EXIT THEN  t T-RES PARAM>FAM FIELD-FAM @ = ;`
  — identity by reserved **family-id**, guarded on `T-RES` first.
- `FIELD-REC/NAME/INNER` = `0/1/2 PARAM>ARG` (`checker.f:540-547`).
- `FIELD-ID-SAME?` (`:555-557`), `FIELD-PAIR?` (`:559-563`), `FIELD-COERCE?`
  (`:565-569`) — the field unify arms in `U-TYPE` (dispatched at
  `checker.f:911-912`). `LIN-TYPE-COUNT*` descends `field` inner via
  `t FIELD-PARAM? IF t T-RES FIELD-INNER … RECURSE` (`checker.f:1074`).

**Item-7 relevance:** hidden physical fields are the *same shape* as `field`
(a `T-PARAM` carrying a reserved-scope family-id + a marker), so the `field`
machinery is the working template for the hidden-field kind, and any hidden-kind
predicate must be distinct from `FIELD-PARAM?` (which keys on `FIELD-FAM`).

---

## 2. Checker paths that must know hidden physical fields vs logical rows

### 2a. Signature parse — the primary conversion site
- `MK-PARAM` (`checker.f:388-404`), `MK-PUSH` (`checker.f:417-423`).
- `SIG-FAM?` (`checker.f:1830-1831`) resolves any family regardless of kind —
  **currently kind-blind** (root of the §0 probe).
- `SIG-TYPE` (`checker.f:1954-1977`): builds `family<...>` as one `T-PARAM`;
  `SIG-END-PARAM` (`checker.f:1940-1942`) arity-checks via `TFAM-ARITY*`.
- **`PSTACK` push site** (`checker.f:2028-2033`):
  ```
  2dup VREC-FIND IF  >r 2drop r> VREC-PUSH-FIELDS
  ELSE  drop SIG-TYPE swap MK-PUSH  THEN
  ```
  Line **2032** `drop SIG-TYPE swap MK-PUSH` is the exact `SIG-TYPE MK-PUSH` the
  item text (`PLAN.md:549`) says to replace with `PUSH-LOGICAL`
  (`docs/type-families.md:625-641`). For a `TK-CELL` family / ordinary type,
  `PUSH-LOGICAL` == `MK-PUSH` (unchanged); for a `TFAM-LAYOUT?` family it must
  push hidden fields (or fail-closed) instead.
- Other `SIG-TYPE` callers: `checker.f:2136` (`VREC-PARSE-FIELDS`, stores the
  field type, NOT a `MK-PUSH` — a layout field type here is item-15 territory but
  is a leak site to note); `LOCAL-TYPE` (`checker.f:1868-1869`, called at
  `checker.f:4260`) for `{: x:type :}` locals — layout locals are item 12, but
  item 7's fail-closed binding must still catch a layout local.

### 2b. Friend xt hooks — NEW hook required
Only two forward hooks exist today: `checker.f:357-358`
```
variable TFAM-RESOLVE-XT   0 TFAM-RESOLVE-XT !
variable TFAM-ARITY-XT     0 TFAM-ARITY-XT !
```
wrapped by `TFAM-RESOLVE*`/`TFAM-ARITY*` (`checker.f:444-447`) and installed by
`type-family.f:652-653` (`' TFAM-RESOLVE TFAM-RESOLVE-XT !` etc.).
`PUSH-LOGICAL` (in checker.f) needs the family **kind/layout/slots**, none of
which are reachable yet. Item 7 must add hook cell(s) — e.g.
`TFAM-LAYOUT?-XT` / `TFAM-SLOTS-XT` (mirror `checker.f:444-447`) — and install
them in `type-family.f` alongside `:652-653`. **type-family.f edit is required**
though it is not in item 7's Paths list (Contradiction §C2).

### 2c. Instantiation / copy / replay (thread the hidden marker + family-id)
All four `T-PARAM` arms are hard-unrolled to 4 args and already carry
`PARAM>FAM` in slot `H`:
- `VREC-COPY` param arm `checker.f:1580-1587` (family-id → `VN.H!` at `:1583`).
- `VREC-INST` param arm `checker.f:1656-1663` (reads `VN.H@` fam into `MK-PARAM`
  at `:1662`; reentrant `PARAM-SCR` base at `:1657`).
- `E-COPY` param arm `checker.f:2857-2867` (family-id → `EN.H !` at `:2861`).
- `E-INST` param arm `checker.f:3036-3044` (fam `EN.H @` → `MK-PARAM` at `:3042`).
- `VREC-PUSH-FIELDS` (`checker.f:1667-1675`) is the existing *hidden-field-style*
  expansion for value records: `VREC-INST swap MK-PUSH` per field — the template
  `LAYOUT-PUSH-FIELDS` mirrors.

**Node-slot constraint:** `VN`/`EN` have exactly 8 slots `A..H`
(`checker.f:1432-1448`, `2716-2724`); a 4-arg `T-PARAM` uses A/B=name, C=argc,
D..G=args0-3, **H=family-id** — all 8 consumed. A separate hidden-kind marker
column has **no free node slot** (see §6 R2).

### 2d. Occurrence / linearity walkers + depth guards
- `TY-OCC?` (`checker.f:872-897`) descends param args 0..3 via
  `PARAM-ARG-OR-DUMMY` (`checker.f:889-894`), bracketed by
  `TWALK-DEEPER/TWALK-SHALLOWER`. Hidden-field terms carry the family's args, so
  occurs-check must descend them identically (no change if hidden = `T-PARAM`).
- `LIN-TYPE-COUNT*` (`checker.f:1067-1078`): `T-PARAM` arm only counts through
  `FIELD-PARAM?` inner today (`checker.f:1074`). A **linear-payload layout**
  (`option<own>`, spec §19) is item 11, but item 7 must ensure a hidden
  layout field does not silently count as 0 linear when it wraps a linear
  payload → for reject-only, the fail-closed binding reject (§2e) is the guard.
- `TWALK-MAX-DEPTH` depth backstop `checker.f:862-868` — shared by all three
  walkers; unchanged.

### 2e. The fail-closed binding-reject site (the heart of "reject-only")
`U-TYPE` (`checker.f:899-921`). The var-bind arms:
```
915  over ISVAR IF  over PAY over TY-OCC? IF … ELSE swap PAY TV! THEN ELSE
917  dup  ISVAR IF  dup  PAY  rot tuck TY-OCC? IF … ELSE swap PAY TV! THEN ELSE
919  over TAG T-CON = over TAG T-CON = and IF … 2dup CON-OK? …
```
and the field arms above them (`checker.f:911-912` `FIELD-PAIR?`/`FIELD-COERCE?`).
This is where "hidden fields must not bind to plain scalar/var/con primitives"
(`docs/type-families.md:607-610`, PLAN acceptance `PLAN.md:562-564`) is enforced:
item 7 adds a **hidden-field-detected → `RES-FALSE UOK !`** guard BEFORE the
`ISVAR`/`T-CON` arms, so a one-cell `dup`/`drop`/`swap` (which unifies its `PE-A`
row var against the hidden tag/slot cell in `CHECKER-STEP`, `checker.f:1030-1038`)
rejects instead of binding. `PARAM-PAIR-ARGS` (`checker.f:838-845`) +
`PARAM-FAM-OK?` (`checker.f:835-836`, id-based compare) already give
**same-tail/different-package non-unification** (PLAN acceptance `PLAN.md:562`) for
free — hidden fields inherit it because identity is `PARAM>FAM`, not spelling.

### 2f. Rollback frames — NO new work
Core frame `RBF-REC` (`checker.f:5145-5162`), `RBF-PUSH` (`checker.f:5195-5216`),
`RBF-POP` (`checker.f:5218-5240`) save UEND/SYM/CT/VREC/pkg/DFER marks and drive
the registry hook `REG-EXT-RB-SAVE-XT`/`-RESTORE-XT` (`checker.f:5216,5219`). The
TFAM half (`type-family.f:525-587`, `TFAM-ROLLBACK-SAVE`/`RESTORE`) already saves
`TFAM-N/TF-STR-U/TF-PK-N/SUMV-N/PF-N/LAY-N`. Item 7 introduces **no new registry
counter** (it consumes existing TFAM/SUMV metadata and produces only in-row
`T-PARAM` terms, which live in the `PARAM*` arenas already covered by the arena
grow/relocation machinery). → rollback/snapshot need no item-7 change unless a
new PARAM SoA column is added (§6 R2); if added, `PARAM-ENSURE`
(`checker.f:342-351`) grows it and no rollback counter is involved (PARAM terms
are per-check, not registry rows).

---

## 3. What "reject-only" means operationally

Reject-only = install the hidden-field kind + logical-row expansion **internally**
and make every checked path that would expose a hidden layout row **fail closed**,
while NOT enabling any public/user-callable layout surface (constructors, MATCH,
width-aware stack ops = items 8/9/12). Acceptance `PLAN.md:558-564`;
sequence gate 17g precedes item 12 (`PLAN.md:1031`).

### 3a. Diagnostic / repair classes that exist today
- SGBAD kinds `checker.f:1731-1734`: `SGBAD-SYNTAX/UNKNOWN/BAREPTR/ARITY-KIND`.
  Setters `SGBAD-UNKNOWN!` (`:1788`), `SGBAD-ARITY!` (`:1797`),
  `SGBAD-BAREPTR!` (`:1793`); predicates `:1791/:1795/:1799`.
- `render.f` maps them: `DCODE` (`render.f:258-265`) →
  `E-UNKNOWN-SIGNATURE-TYPE` / `E-BARE-PTR-SIGNATURE` / `E-WRONG-ARITY` /
  `E-BAD-SIGNATURE`; `REPAIR-CLASS` (`render.f:279-287`) →
  `fix_signature_type` / `fix_bare_ptr_element` / `fix_signature_arity` /
  `fix_signature_syntax`; `DIAG-PROSE` (`render.f:346-357`) human text.
- Repair-class contract: `docs/repair-diagnostics.md:103-108,130-133` already
  lists `fix_signature_type`/`fix_signature_arity`/`fix_bare_ptr_element`.

### 3b. `@`-names already reject TODAY (probed, fail-closed)
```
$ printf ': B ( @result.tag -- n ) drop 0 ;' | bin/hb --load /dev/stdin
habu: in b: unknown type '@result.tag' in signature      # exit 70
```
`@result.tag` never resolves (`TFAM-RESOLVE` returns false for `TF-HIDDEN?`,
`type-family.f:280`), falls through `TOK-TYPE`→`BAD-SIG-TYPE`
(`checker.f:1802-1804`) → `SGBAD-UNKNOWN` → code `E-UNKNOWN-SIGNATURE-TYPE`,
`repair_class fix_signature_type`. Also probed: `span<n>` →
`E-WRONG-ARITY`/`fix_signature_arity` (exit 70); `nope<n>` → unknown (exit 70);
good `span<space-global,f32,extent-n>` → exit 0.

### 3c. What new code/repair_class item 7 requires
- **No new repair_class is strictly required by item 7.** The `@`-name public
  reject already fires (§3b) via the generic unknown-type class, satisfying
  acceptance "public `@result.tag` signatures reject" (`PLAN.md:559`). A
  *dedicated* "hidden field type is not public" diagnostic
  (`docs/type-families.md:1461`) is **item 13's** job ("extend SGBAD/diagnostic
  state … ADT fields", `PLAN.md:851-869`), not item 7.
- **New checker code item 7 does require:** the layout-family binding-reject in
  `U-TYPE` (§2e). That reject surfaces through the existing rejection machinery
  (`UOK !` → checker verdict `rejected`, code `E-REJECTED`/`E-MISMATCH`), so it
  reuses the current diagnostic surface with no new repair_class in item 7.
  (Recommended dot the dedicated hidden-field class for item 13 if not already
  tracked.)

### 3d. Where the negative fixtures live
- **JSON diagnostic fixtures:** `test/gate-diagnostics-lib.f` — pattern
  `GDX-UNKNOWN-SIGNATURE` (`:323-331`), `GDX-BARE-PTR-SIGNATURE` (`:333-340`),
  `GDX-BAD-PARAM-SIGNATURE` (`:354-362`), each `GE-SRC-LINE` + `GDX-CHECK-JSON` +
  `GDX-EXPECT-ERR-JSTR`. Item 7 adds a `@result.tag`-reject fixture and a
  layout-binding-reject fixture here (contract: `docs/repair-diagnostics.md`).
- **Engine-run registry/behavior suites:** `test/type-family-suite.f`
  (299 lines, run `bin/hb < test/type-family-suite.f`; `T=`/`T$=`/`REPORT`
  harness; already registers families via top-level `TFAM-DECL`) and
  `test/type-family-rollback-suite.f`. Item 7's "register a `TK-SUM` family,
  reference it in a checked def, assert reject" fixtures belong here (they can
  bypass the missing item-6 grammar using friend-only `TFAM-DECL`/`TFAM-SLOTS!`).

---

## 4. Acceptance criteria as a checklist (files/words + 5/6 deps)

From `PLAN.md:558-564`:

1. **"constructor signatures render as logical types"** — render path
   `PARAM-START` (`render.f:102-103`), `QREND` T-PARAM arm
   (`render.f:155-165`). *Reject-only note:* real generated constructors are
   item 8; for item 7 this means the renderer must not emit raw `@family.slotN`
   for a logical family token (compaction is item 13, `PLAN.md:851-853`). Item 7
   at most keeps logical `family<args>` rendering intact.
2. **"internal rows contain hidden fields"** — new `PUSH-LOGICAL` /
   `LAYOUT-PUSH-FIELDS` in checker.f replacing `checker.f:2032`; hidden field =
   `T-PARAM` with layout `PARAM>FAM` + `@family.slotN`/`@family.tag` name +
   hidden marker; count = `TFAM-SLOTS@ + 1` (`type-family.f:200`).
3. **"public `@result.tag` signatures reject"** — already holds (§3b);
   item 7 must keep it after adding `PUSH-LOGICAL`. Touches
   `SIG-FAM?`/`SIG-TYPE`/`TOK-TYPE` (`checker.f:1830,1954,1858`),
   `TFAM-RESOLVE`+`TF-HIDDEN?` (`type-family.f:278-283,96-98`).
4. **"existing `field<...>` behavior subsumed or preserved w/ compat tests"** —
   `FIELD-PARAM?`/`FIELD-PAIR?`/`FIELD-COERCE?`/`FIELD-INNER`
   (`checker.f:535-569`), `VREC-PUSH-FIELDS` (`checker.f:1667`),
   `VREC-FIELD-WRAP` (`checker.f:2094-2100`). Preserved because `field` is
   `TK-CELL` (`type-family.f:648`) → not `TFAM-LAYOUT?`. Compat tests:
   `test/type-family-suite.f` + any VREC engine fixture.
5. **"same-tail hidden fields from different packages cannot unify/compact"** —
   `PARAM-FAM-OK?`/`PARAM-PAIR-ARGS` (`checker.f:835-845`), id-based, already
   correct; hidden fields inherit via `PARAM>FAM`.
6. **"before item 12, any path exposing hidden layout rows rejects (no one-cell
   primitive touch)"** — the `U-TYPE` fail-closed guard (§2e, `checker.f:911-921`)
   + `CHECKER-STEP` (`checker.f:1030-1038`).

**Consumes from item 6** (`PLAN.md:509-544`, UNBUILT — see §0/§C1): the
`SUMTYPE`/`TYPEFAMILY` defining words that populate `TF.KIND=TK-SUM/TK-ENUM`,
`TF.SLOTS` (via `TFAM-SLOTS!`), `SUMV` variant rows (via `SUMV-ADD`), and reserve
the new tokens. Item 7 reject-only can substitute **friend-only `TFAM-DECL`
+ `TFAM-SLOTS!`** in tests, so it is buildable ahead of item 6, but its gate
17g "full" proof presumes item-6 declared families.

**Consumes from item 5** (`PLAN.md:409-507`): the ordered source-composition
event log + all-errors/preverify replay so a bad layout signature reports a
diagnostic and rolls back without poisoning later defs (`PLAN.md:431-433`,
`475-479`). Concretely item 7 relies on item 5's replay preserving family
metadata order so a `family<...>` signature resolves the family before use; item
7 adds no new event kinds.

---

## 5. Trust surface

`rg 'TRUSTED:|\bTRUST\b|set-check'` over the touched files:
- `checker.f:23,1010,2242,2570` — pre-existing `TRUSTED:` raw-cell→pointer arena
  converters (`ARENA-RC>PTR`, `TOKBUF-RC>PTR`, `USIGS-RC>PTR`, `HIDX-RC>PTR`),
  unrelated to ADTs.
- `checker.f:4906-4916` — the checker's own `TRUST` definer (the boundary word),
  not a trust *row*.
- `render.f:302` — a `suggestion` string literal, not a trust site.
- `type-family.f`, `type-schema.f` — **zero** trust rows.

**Item 7 needs NO new trust rows.** Its changes are pure checked-checker logic
(parse branch, unify guard, term walk). This satisfies the 17g blocking rule:
"The type-family/ADT campaign may not add `TRUST`, `TRUSTED:`, `set-check`, or
`TRUSTED.md` rows" (`PLAN.md:990-992`). No gap. (These core files load in the
checker bootstrap prefix and are not themselves checked user code, so there is no
new *unchecked boundary* introduced either.)

---

## 6. Open risks / unknowns (each with a probe)

- **R1 — Kind-blind `SIG-FAM?`.** Every registered family resolves in a signature
  regardless of kind, so a layout family silently parses one-cell (§0 probe).
  Item 7 must add a layout branch. *Probe:* the §0 `myresult<n,n>` snippet must
  flip from exit 0 to a reject after the change; re-run identically.
- **R2 — No storage for a hidden-kind marker.** All 8 term tags are used
  (`checker.f:1-3`) → no new 3-bit term kind; and `VN`/`EN` slots A..H are all
  consumed by a 4-arg `T-PARAM` (H=family-id, `checker.f:1583,2861`). A hidden
  marker must be either (a) name-based (`@`-prefix via `TF-HIDDEN?`,
  `type-family.f:96`) — cheapest, but then persistence/rendering must treat the
  `@`-name as internal-only — or (b) a NEW `PARAM*` SoA column threaded through
  `MK-PARAM`/`PARAM-ENSURE` (`checker.f:342-404`) and all four replay arms
  (§2c), which then also needs a VN/EN node slot the nodes don't have. *Probe:*
  grep the node writers — `rg -n 'VN\.H!|EN\.H !' src/core/checker.f` shows H is
  the last free slot and already holds family-id; confirm before choosing (a).
  **Decision owner: implementer; recommend (a) name-based marker, identity still
  family-id.**
- **R3 — `TF.SLOTS` is 0 with no grammar.** Layout expansion width comes from
  `TFAM-SLOTS@` (`type-family.f:200`), never set (item 6/8). *Probe:*
  `printf 's" " CHECKER-PACKAGE-PUBLIC s" s" 2 TK-SUM TFAM-DECL 1 TFAM-SLOTS!' |
  bin/hb` then read back `TFAM-SLOTS@` — reject-only tests MUST set it explicitly.
- **R4 — Diagnostic leak of `@family.slotN`.** Item 7 risk note
  (`PLAN.md:565-566`): without compaction (item 13) an internal row rendered in a
  diagnostic could show `@result.slot0`. *Probe:* after wiring `PUSH-LOGICAL`,
  craft a reject whose diagnostic renders the row (`REND-TYPE`, `render.f:169`)
  and confirm no `@`-name reaches stderr; if it does, either defer expansion to
  after the reject point or land minimal render suppression (documented as the
  item-13 seam, not a full compactor).
- **R5 — `LOCAL-TYPE` layout local.** `{: x:myresult<n,n> :}` reaches
  `LOCAL-TYPE` (`checker.f:1868`, call `:4260`), not the `PSTACK` push site.
  *Probe:* `printf '… TFAM-DECL drop\n: L ( n -- n ) {: x:myresult<n,n> :} 0 ;' |
  bin/hb` — confirm it also rejects (must, per acceptance "any checked path");
  the `U-TYPE` guard (§2e) should catch the local bind if `LOCAL-TYPE` builds the
  same layout `T-PARAM`.
- **R6 — `VREC-PARSE-FIELDS` layout field.** A value-record field typed as a
  layout family (`checker.f:2136` `SIG-TYPE`) is item-15 scope but a reachable
  leak today. *Probe:* declare a `TK-SUM` family, then a `VALUE-RECORD` with a
  field of that type; confirm item 7's guard (or an explicit reject) fires rather
  than silently wrapping a layout inside `field<...>`.

---

## Contradictions

- **C1 — Dependency (item 6) is unbuilt.** Item 7 "Depends on: item 6"
  (`PLAN.md:568`) and the order `6 -> 17f -> 7` (`PLAN.md:1031`) presume the
  `SUMTYPE`/`TYPEFAMILY` grammar exists, but no such defining word exists in the
  tree (§0). The registry mutators (`SUMV-ADD`, `TFAM-SLOTS!`, `LAY-ADD`) are
  defined but uncalled. Item 7 is therefore only *testable* via friend-only
  `TFAM-DECL`/`TFAM-SLOTS!` synthetic registrations until item 6 lands; its gate
  17g "full" proof cannot use real `SUMTYPE result … END-SUMTYPE` fixtures yet.
  Not silently resolved — flagged for the orchestrator: either build item 6 first
  (matches the stated order) or accept synthetic-registration fixtures for the
  reject-only slice.
- **C2 — Paths list omits `type-family.f`.** Item 7 Paths =
  `checker.f, render.f, docs/type-families.md` (`PLAN.md:547-548`). But
  `PUSH-LOGICAL` lives in checker.f (loads BEFORE type-family.f) and needs a
  family **kind/layout/slots** query, reachable only through a forward xt cell
  installed by `type-family.f` (mirroring `type-family.f:652-653`). So item 7
  **must edit `type-family.f`** to install a new `TFAM-LAYOUT?-XT`/`TFAM-SLOTS-XT`
  hook — a file outside its declared Paths. Either the Paths list is incomplete
  or the item intends `checker.f` to reach layout state another way (none exists:
  checker.f cannot name `TFAM-LAYOUT?`, defined 200+ lines later in a
  later-loading file). Recommend adding `src/core/type-family.f` to item 7 Paths.
- **C3 — "reject-only" vs "internal rows contain hidden fields".** Acceptance
  simultaneously requires hidden fields to exist on internal rows
  (`PLAN.md:559`) and requires any path exposing them to reject
  (`PLAN.md:562-564`). These are consistent only if expansion happens but the
  expanded cells are *unbindable* by ordinary primitives (§2e) — i.e. expansion
  is internal-and-inert, not "don't expand". The implementer must not read
  "reject-only" as "reject the whole layout signature at parse time" (that would
  fail the "internal rows contain hidden fields" clause). Noted so the two
  clauses are not traded off against each other.
