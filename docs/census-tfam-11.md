# Item 11 Census — Linear/Resource Semantics For Layout Values

Dot: `habu-tfam-11-linear-99fa9990`. PLAN.md item 11 (`PLAN.md:742-758`), per-item
gate 17l (`PLAN.md:958-1027`; order `... 10 -> 17k -> 11 -> 17l -> 13 ...`,
`PLAN.md:1031`). Normative spec `docs/type-families.md` §19 Linear/resource
interaction (`:1168-1196`), §10 hidden fields (`:607-610`), §11 logical row
expansion (`:614-641`), §24 diagnostics (`:1453-1476`).

Every claim is `file:line` + a quoted definition/snippet. **Symbols are
authoritative; line numbers drift — anchor to the quoted symbol.** Paths absolute
in the summary; relative here for density (repo root `/Users/joel/Work/habu`).

Term encoding (re-verified against census-tfam-4/7): 3-bit `TAG = x and 7`, `PAY =
x >> 3`; tags `0 T-CON 1 T-VAR 2 T-PTR 3 S-ROW 4 S-PUSH 5 T-QUOT 6 T-ATOM 7
T-PARAM` (`checker.f:1-3`). A resolved-type accessor `T-RES ( n -- n )` compresses
the var chain before any `PAY`/`PARAM>ARG` read.

---

## 0. State of the world (what item 11 builds ON, and what is missing)

Item 11 `Depends on: items 7-10 and 12` (`PLAN.md:757`). **None of those
dependencies are landed in this tree.** Evidence:

- `dot list`: `habu-tfam-7-hidden`, `-8-generated`, `-12-layout` are **open**
  (`o`); `-9-construct`, `-10-native` are **in-progress** (`>`); `-11-linear`
  (this dot) is open.
- **No layout ADT value can exist on a stack yet.** Repo-wide search for the
  item-9/11/12 surface returns only the spec/PLAN and unrelated substring hits:
  `LAYOUT-LINEAR*`, `LAYOUT-PUSH-FIELDS`, `PUSH-LOGICAL`, a `MATCH` control form,
  `CF-MATCH`, and a `construct` definer are **absent** from `src/ lib/ test/
  tools/ bootstrap/`. `SUMV-ADD` (`type-family.f:375`) is called only by friend
  tests (`test/type-family-suite.f:200-205`, `test/type-family-rollback-suite.f`),
  never by a grammar. Every registered family today is `TK-CELL`
  (per census-tfam-7 §0); `TK-SUM`/`TK-ENUM`/`TK-PRODUCT` have zero instances.

**Consequence.** Item 11 is *design-complete but not integration-testable end to
end* until 6/7/8/9/12 land. Like item 7, its core additions
(`LAYOUT-LINEAR?`/`LAYOUT-LINEAR-COUNT` + the bundle-aware count) are **buildable
and unit-testable friend-only** against synthetic `TFAM-DECL` + `TFAM-SLOTS!` +
`SUMV-ADD` registrations plus `DEFLINEAR`, exactly the harness
`test/type-family-suite.f` already uses. See §5.

**The one thing that DOES exist as a working multi-cell-linear precedent:** a
`VALUE-RECORD` with a linear field. `test/engine-suite.f:935`
`VALUE-RECORD hdl owner own raw ptr u8 END-VALUE-RECORD` — two physical cells,
one of them the linear con `own`. It is the closest analogue to a layout bundle
today, and its linear behavior is proven by fixtures (`:960` `COK-HDL-PASS ( hdl
-- hdl )` certifies; `:965` `CBAD-HDL-DUP ( hdl -- hdl hdl ) over over` rejects).
**Read §2 for why VREC's per-cell counting does NOT generalize to sum layouts.**

---

## 1. The existing linear machinery (the base item 11 extends)

### 1a. Declaration surface — `deflinear` → `CT-LINEAR`
- User word: `roles.f:119-121`
  `: DEFLINEAR ( -- )  parse-name dup 0= IF s" deflinear: missing name" 70 die THEN  CHECKER-DEFLINEAR ;`.
  Preverify/tool mirrors: `verify-source.f:350-353` (`RECORD-DEFLINEAR`),
  `:379` dispatch; `check-core.f:656` (`name LEX-TOK CHECKER-DEFLINEAR`),
  `:644`,`:732`. Trusted boundary sig for the primitive: `check-core.f:20`
  `s" CHECKER-DEFLINEAR" s" ptr u8 n --" TRUST`.
- Checker entry: `checker.f:4035-4036`
  `: CHECKER-DEFLINEAR ( ptr u8 n -- )  CT-ADD-LINEAR ;` (prim decl
  `checker.f:3458`).
- Registration: `checker.f:1854-1857`
  ```
  : CT-ADD-LINEAR ( ptr u8 n -- ) {: a:ptr u:n :}
     a u TYPE-RESERVED? IF s" checker: bad or duplicate signature type" 70 die THEN
     a u CTN @ CT-LINEAR 64 CS-NONE CT-SET
     LIN-NDECL @ 1 + LIN-NDECL ! ;   \ un-gate the linear kind discipline
  ```
  A linear type is a con-table entry with class `CT-LINEAR` (`checker.f:614`
  `6 constant CT-LINEAR`), tested by `CT-LINEAR? ( n -- bool )`
  (`checker.f:782-783`). **Registering ANY linear increments `LIN-NDECL`**, the
  global gate.

### 1b. The gate — everything is off unless a linear is in scope
`checker.f:187-188`
`variable LIN-NDECL   0 LIN-NDECL !` / `: LIN-ANY? ( -- bool ) LIN-NDECL @ 0 <> ;`.
Rationale (`checker.f:185-186`): "gated on any DEFLINEAR type being declared, so
non-linear code (the entire self-build) pays nothing." `LIN-ANY?` is checked at
the head of `LIN-TAINT-SCAN` (`:1053`) and `LIN-EFF-PASS` (`:3109`). **Note the
count-conservation path (`LIN-CHECK`, §1d) is NOT gated on `LIN-ANY?`** — it runs
every step but is a no-op when no linear con is on the stack.

### 1c. `LIN-CON?` and the per-cell / per-row counters (the exact extension site)
- `checker.f:1044-1046`
  `: LIN-CON? ( n -- bool )  T-RES dup TAG T-CON <> IF drop RES-FALSE EXIT THEN  PAY CT-LINEAR? ;`
  — resolves a type and returns true only for a concrete linear con.
- `checker.f:1067-1077` `LIN-TYPE-COUNT*` — the recursive per-type linear count.
  The **`T-PARAM` arm is the load-bearing line for item 11**:
  ```
  T-PARAM of
     t FIELD-PARAM? IF t T-RES FIELD-INNER TWALK-DEEPER RECURSE TWALK-SHALLOWER ELSE 0 THEN
  endof
  ```
  It descends into a `field<rec,name,inner>` VREC wrapper (`FIELD-PARAM?`,
  `checker.f:535-538`, identity by reserved `FIELD-FAM`) — **but ANY OTHER
  `T-PARAM` (including a hidden layout field whose `PARAM>FAM` is a sum/enum/
  product family id, NOT `FIELD-FAM`) falls to the `ELSE 0`**. A linear payload
  nested inside a layout bundle therefore counts as **zero** today (§2, R1).
- `checker.f:1078` `: LIN-TYPE-COUNT ( n -- n ) TWALK-RESET LIN-TYPE-COUNT* ;`.
- `checker.f:1080-1087` `LIN-ROW-COUNT ( row -- n )` sums `LIN-TYPE-COUNT` over
  every `S-PUSH` cell in a row. **This is the exact word `LAYOUT-LINEAR-COUNT (
  row -- n )` (spec `:1193`) must become or wrap** — it must count each linear
  layout bundle **once** and skip the bundle's remaining `M` physical cells, not
  sum per cell.
- `checker.f:1089-1090` `LIN-TOTAL ( n n -- n )` = `LIN-ROW-COUNT` of data row +
  return row.

### 1d. Enforcement path A — concrete-count conservation (`LIN-CHECK`)
`checker.f:1092-1099`:
```
: LIN-SNAPSHOT ( -- ) DCUR @ RCUR @ LIN-TOTAL LINBEF ! ;
: LIN-EXPLICIT? ( n n -- bool ) LIN-TOTAL 0 <> ;
: LIN-CHECK ( -- ) DCUR @ RCUR @ LIN-TOTAL LINBEF @ <> IF 0 OK ! THEN ;
```
Driven by `CHECKER-STEP` (`checker.f:1101-1109`): a step whose *declared* rows do
NOT name a linear con (`LINEXP @ 0=`) must **conserve** the linear count across the
step; a `dup`/`over` of a linear (poly `(a -- a a)`) raises the count → reject; a
`drop` lowers it → reject. This is why `CBAD-OWN-DUP`/`-DROP`/`-OVER`/`-FETCH`/
`-STORE` (`engine-suite.f:898-902`) reject and `COK-OWN-PASS` (`:895`) certifies.

### 1e. Enforcement path B — polarity multiplicity (`LIN-EFF-PASS`)
`checker.f:3048-3125`. `EN-MULT` (`:3056-3090`) tallies a canonical effect var's
occurrences split by polarity (a quotation argument flips polarity, `:3076-3078`);
`LIN-VAR-MULT` (`:3092-3100`) returns `neg pos`; `LIN-EFF-PASS ( h -- )`
(`:3108-3125`) rejects when a var that resolved to a linear con has unequal
in/out multiplicity `(a)`, else taints a still-polymorphic non-linear-use var
`(b)`. **Called at two sites:** `EFF-APPLY` (`checker.f:3136`, every word/prim
application) and `CF-RECURSE-EFF` (`checker.f:4482`, recursive self-call). Catches
`KEEP`/`BI` laundering (`engine-suite.f:924-925`).

### 1f. Enforcement path C — deferred taint (`LIN-TAINT` / `LIN-TAINT-SCAN`)
Taint list storage `checker.f:194-208` (`LIN-TAINT ( n -- )` appends a canonical
var id). Scan `checker.f:1052-1059`:
```
: LIN-TAINT-SCAN ( -- )
   LIN-ANY? 0= IF exit THEN   OK @ 0= IF exit THEN   0 LTNT-I !
   BEGIN LTNT-I @ LTNT-N @ < WHILE
      LTNT-I @ cells LTNT + @ MK-VAR LIN-CON? IF 0 OK ! THEN
      LTNT-I @ 1 + LTNT-I ! REPEAT ;
```
Runs after **every token** at the tail of `DO-TOK1` (`checker.f:4998`). Rejects a
var copied/dropped while polymorphic that LATER binds linear (`[: dup FREE ;]`,
`engine-suite.f:926-927`).

### 1g. Return-stack transfers — their own `LIN-SNAPSHOT`/`LIN-CHECK`
`RS->R`/`RSR>`/`RSR@`/`RS2->R`/`RS2R>`/`RS2R@` (`checker.f:1114-1156`) each bracket
their transfer with `LIN-SNAPSHOT` … `OK @ IF LIN-CHECK THEN`. Because `LIN-TOTAL`
counts **both** rows, moving a linear data↔return is neutral; leaking it on the
return row is caught by the return-row balance check (proven by probe P6, §8).

### 1h. Quotation application — `RSEXEC` / `RSCATCH`
- `RSEXEC` (`checker.f:1174-1204`): captures `RSEXEC-LIN-EXPLICIT?`
  (`:1168-1170`) BEFORE `UNIFY-IN` (comment `:1181-1182`), then
  `OK @ RSEXEC-EXP @ 0= and IF LIN-CHECK THEN` (`:1193`). Proven by
  `COK-OWN-EXEC-*`/`CBAD-OWN-EXEC-*` (`engine-suite.f:906-912`).
- `RSCATCH` (`checker.f:1208-1232`): **does NOT call `LIN-CHECK`.** It relies on
  the pre/post rows unifying (`Q>DIN`/`Q>DOUT`, `:1218,1223`). A `catch` frame
  preserving a linear is neutral by construction (probe P7 certifies). Item 11
  must confirm this holds for a *layout* bundle crossing a `catch` edge — that is
  item 12's `catch`/`throw` metadata requirement (`PLAN.md:833,837-839`), not new
  item-11 code, but item 11's fixtures should cover a linear-layout-through-catch.

### 1i. TWALK depth guards on the linear walker (recent hardening)
`checker.f:851-868`. `$2000 constant TWALK-MAX-DEPTH` (`:862`), `TWALK-RESET`
(`:864`), `TWALK-DEEPER` (`:865-867`, dies `"checker: term walk too deep (cyclic
term)"` code 76), `TWALK-SHALLOWER` (`:868`). Shared by `TY-OCC?`, `E-COPY`, and
**`LIN-TYPE-COUNT`** (comment `:852-853`). The linear walker charges depth only on
its one recursive descent — the `FIELD-PARAM?` inner (`checker.f:1074`). **Item 11
adds a SECOND descent** (into layout variant payload schemas / bundle inner types);
that new recursion **must be bracketed by `TWALK-DEEPER`/`TWALK-SHALLOWER`** or a
recursive/cyclic layout family (`option<option<...>>`, spec §18 layout-poly) can
overflow the native stack. Cyclic-layout rejection is item 16's `POLICY`
(`type-families.md:1473` "invalid layout policy for recursive sum"), but the
depth backstop is item 11's obligation the moment its walker descends a schema.

### 1j. Rollback of the gate
`checker.f:5145-5162` `BEGIN-STRUCTURE RBF-REC` includes `CELL +FIELD RBF.LIN`
(`:5152`); saved `RBF-PUSH` `checker.f:5204` `LIN-NDECL @ r RBF.LIN !`; restored
`RBF-POP` `checker.f:5229` `r RBF.LIN @ LIN-NDECL !`. So a scoped/candidate
`DEFLINEAR` un-gates and re-gates transactionally. **Item 11 adds no new registry
counter** (`LAYOUT-LINEAR?` reads existing TFAM/SUMV/SCHEMA rows already covered by
`TFAM-ROLLBACK-SAVE/RESTORE`, `type-family.f:525-587`); it only reads `LIN-NDECL`
via `LIN-ANY?`. No new rollback field required.

---

## 2. What "layout values" adds over today's single-cell linear nominals

Today a linear value is **one physical cell** carrying a bare `T-CON` of class
`CT-LINEAR` (`own`, `checker.f:614`). `LIN-TYPE-COUNT`'s `T-CON` arm counts it
directly (`checker.f:1069`). Copying/dropping that one cell changes the total →
`LIN-CHECK` fires.

A **layout value** (item 7/12) is a **multi-cell bundle**: `M` payload slots + 1
tag, expanded by `PUSH-LOGICAL`/`LAYOUT-PUSH-FIELDS` (spec `:614-641`) into `M+1`
**hidden `T-PARAM` cells** (`@family.slotN` / `@family.tag`), where
`M = TFAM-SLOTS@` (`type-family.f:200`). The family is `TFAM-LAYOUT?`
(`type-family.f:218-219`, true for sum/enum/product). Two structural differences
break the scalar model:

1. **The linear is nested, not bare.** The payload's linear type sits INSIDE a
   hidden `T-PARAM` slot cell, not as a top-level `T-CON`. `LIN-TYPE-COUNT`'s
   `T-PARAM` arm only descends `FIELD-PARAM?` (VREC `field`), so a layout slot
   returns **0** (§1c). → A linear layout would count as 0 and be freely
   copy/dropped. **This is the central unsoundness item 11 closes** (PLAN risk
   `:754-755` "layout expansion and linear counting must agree or linear payloads
   can be laundered through hidden fields").

2. **A sum is a tagged union — payload presence is variant-dependent.** For
   `option<own>`, slot0 holds the linear only in the `some` variant; in `none`
   slot0 is padding (spec `:1183-1186`). Per-cell counting over the raw `M+1`
   expansion is therefore **wrong on two axes**: it would (a) miss the linear when
   the slot type is padding/param-unresolved, and (b) risk double-counting if
   more than one slot resolves linear. The spec mandate is **bundle-granular**:
   `LAYOUT-LINEAR? ( type -- bool )` = true iff the family is linear (any variant
   *may* contain a linear payload, spec `:1172`), and `LAYOUT-LINEAR-COUNT ( row
   -- n )` counts **one per linear bundle, skipping the bundle's `M` remaining
   cells** — "consume-once across a multi-cell bundle, not per cell."

**Why VREC does not generalize.** `VALUE-RECORD hdl` (`engine-suite.f:935`)
expands via `VREC-PUSH-FIELDS` (`checker.f:1667`) into **bare cells** `[own, ptr
u8]` — the `own` is a top-level `T-CON`, counted directly, so `over over`
(`:965`) copies it and `LIN-CHECK` rejects. A product record has all fields always
present, so per-cell counting is sound. A **sum** deliberately keeps the bundle as
*unbindable hidden `T-PARAM` cells* (item 7 reject-only; item 12 width-aware) so
one-cell prims cannot touch it — which is exactly why the linear must be counted
at the **bundle** level, by resolving the family's variant payload schemas, not by
reading a bare cell type.

**How item-12 layout-aware ops compose with linearity.** Item 12 makes `dup`,
`drop`, `over`, `swap`, `nip`, `rot`, `2dup`, … operate on whole logical bundles
(`PLAN.md:764-770`) and makes return-stack transfers / locals width-aware
(`:773-775`). Once a width-aware `dup` moves `M+1` cells as one unit,
`LAYOUT-LINEAR-COUNT` seeing a +1 bundle → `LIN-CHECK` rejects the copy; a
width-aware `drop` of a live linear bundle → -1 → reject. **Item 11 supplies the
count; item 12 supplies the atomic movement. They must agree on bundle boundaries
or the count is off** (§8 R2).

---

## 3. Every checker path that enforces/propagates linearity + bundle-awareness need

| # | Path (word, `checker.f`) | Mechanism | Needs bundle-awareness? |
|---|---|---|---|
| A | `CHECKER-STEP` (`:1101`) via `LIN-SNAPSHOT`/`LIN-EXPLICIT?`/`LIN-CHECK` (`:1092-1099`) | count conservation per prim step | **YES** — `LIN-TOTAL`→`LIN-ROW-COUNT`→`LIN-TYPE-COUNT` must count layout bundles once (§1c, R1) |
| B | `EFF-APPLY` (`:3127`) → `LIN-EFF-PASS` (`:3108`); `EN-MULT` (`:3056`) | polarity multiplicity through vars | **YES if a type var binds a layout family** — a poly quotation over a linear layout must reject copy/drop; `EN-MULT`'s `EN-PARAM` arm (`:3082-3088`) must treat a linear-layout param as one linear unit |
| C | `CF-RECURSE-EFF` (`:4474`) → `LIN-EFF-PASS` (`:4482`) | same, for recursion | YES (same as B) |
| D | `LIN-TAINT` (`:205`) / `LIN-TAINT-SCAN` (`:1052`, called `DO-TOK1:4998`) | deferred taint via `LIN-CON?` | **YES** — a layout var tainted then later resolving to a linear *layout* must reject; scan uses `LIN-CON?` which is con-only → needs a `LAYOUT-LINEAR?` companion test |
| E | `RS->R`…`RS2R@` (`:1114-1156`) | return-stack transfer count | **YES** — via `LIN-TOTAL`; plus item-12 width-aware `>r`/`2>r` must move whole bundles (`PLAN.md:773-774`) |
| F | `RSEXEC` (`:1174`) `RSEXEC-LIN-EXPLICIT?` (`:1168`) | quotation apply count | **YES** — explicitness test uses `LIN-TOTAL` on the quot rows; a quot naming a linear layout is "explicit" |
| G | `RSCATCH` (`:1208`) | pre/post row unify (no `LIN-CHECK`) | **NO new count code**, but fixtures must prove a linear layout survives a `catch` edge (item-12 metadata, `PLAN.md:833`) |
| H | `LOC-BIND` (`:4294`, `CHECKER-STEP:4299`) + `LOC-REF?` (`:4314`) | locals `{: :}` bind/ref | **YES for width (item 12) — AND a PRE-EXISTING linear hole exists here (§8 R4 / Contradiction C1).** Layout locals also blocked by the single-token local-type parser (probe Q5) |
| I | `LIN-TYPE-COUNT*` `T-PARAM` arm (`:1073-1075`) | per-type descent | **YES — the primary new-code site.** Add a layout-family branch (count 1, do NOT descend `M` slots per cell) bracketed by `TWALK-DEEPER/SHALLOWER` |

**Words unchanged in shape but re-pointed at bundle counting:** `LIN-ROW-COUNT`
(`:1080`), `LIN-TOTAL` (`:1089`), `LIN-SNAPSHOT` (`:1092`), `LIN-CHECK` (`:1098`),
`LIN-EXPLICIT?` (`:1095`), `RSEXEC-LIN-EXPLICIT?` (`:1168`) — all reach linearity
solely through `LIN-TYPE-COUNT`/`LIN-CON?`, so fixing those two primitives (add
`LAYOUT-LINEAR?`-awareness) propagates to every consumer without touching the
consumers. This is the minimal-surface path item 11 should take.

---

## 4. MATCH interaction (item 9) — single-consumption + per-arm refinement

Spec §19 rules (`type-families.md:1183-1187`): constructing consumes the linear
payload and produces a linear sum (1); **matching a linear sum consumes the sum
exactly once** (2); the payload-bearing branch receives the linear **exactly once**
(3); the empty branch receives no payload (4); **dropping a linear sum is rejected
unless a match/destructor consumes payloads correctly** (5). Spec `:1196`: "`MATCH`
must be a checker control form partly because it is the only sound way to refine
linear payloads per variant." Diagnostic `type-families.md:1472` "linear payload
requires explicit match/destructor".

**What the checker must track (all NEW, item 9 owns the frame, item 11 the linear
accounting inside it):**
- `MATCH` does not exist yet (§0). Item 9 adds `CF-MATCH` frames with "family id,
  type args, base rows, seen variants, branch output rows, dead-path state"
  (`PLAN.md:660-662`). Item 11 must make consuming the scrutinee count as the
  **single** linear consumption of that bundle (a `LIN-CHECK`-visible -1 at match
  entry), and each arm's refined payload count as **one** available linear that
  the arm must consume/forward exactly once (the branch-output-row balance already
  planned at `PLAN.md:678`, extended to linear-count equality across arms).
- **Reject re-use of the payload in an arm** (using it twice, or dropping it): the
  per-arm row must run the same `LIN-CHECK`/`LIN-EFF-PASS` discipline as a normal
  body, seeded with the refined payload as the arm's linear input.
- Until item 11 lands, `MATCH` over linear/possibly-linear layouts **rejects
  wholesale** (`PLAN.md:666-668`, `:679-680`); item 11 flips those fixtures from
  reject to "exact branch consumption" (`PLAN.md:751`, `:680`).

**Item 11's `MATCH` code is entirely forward-dependent on item 9's frame.** It
cannot be written before `CF-MATCH` exists; the friend-only slice item 11 CAN land
early is `LAYOUT-LINEAR?`/`LAYOUT-LINEAR-COUNT` + the stack-op/`execute`/RS/taint
count extension (§5).

---

## 5. Dependencies — what item 11 consumes, what is buildable earlier

**Consumes from item 7** (`PLAN.md:546-569`): the hidden-field `T-PARAM`
representation + `PUSH-LOGICAL` expansion and the `TFAM-LAYOUT?-XT`/`TFAM-SLOTS-XT`
friend hooks (census-tfam-7 §2b/C2). Item 11's `LIN-TYPE-COUNT*` layout branch
keys on the **same hidden-field identity item 7 chooses** (name-`@`-marker vs new
SoA column, census-tfam-7 R2) — if that identity is undecided, item 11's detector
is undecided (§8 R1).

**Consumes from item 8** (`PLAN.md:571-644`): schema→type instantiation so
`LAYOUT-LINEAR?` can resolve a variant payload schema (`SV.SCH-START/COUNT`,
`type-family.f:328-329`; schema nodes `SCH-CON`/`SCH-PARAM`/`SCH-APP`,
`type-schema.f:24-28,90-98`) against the value's actual type args and test the
result with `LIN-CON?`. Two linearity sources: a payload declared as a concrete
linear (`SCH-CON` with a `CT-LINEAR` con) and a payload that is a **type parameter**
(`SCH-PARAM`) instantiated with a linear at the use site (`result<ptr u8,own>`,
spec `:1178`). `LAYOUT-LINEAR?` must handle both — hence it needs item 8's
instantiation, not just the raw schema. Item 8 itself blocks its linear-payload
constructors on item 11 (`PLAN.md:602-604,638-639`) — a **staged handshake**, not a
cycle: item 8 lands reject-only for linear payloads, item 11 flips them to
publish+consume.

**Consumes from item 9** (`PLAN.md:646-688`): the `CF-MATCH` frame (§4).

**Consumes from item 12** (`PLAN.md:760-844`): atomic whole-bundle movement for
every stack primitive, return-stack transfer, and locals bind/ref, plus the
"reject possibly-linear layout copy until item 11" gate (`PLAN.md:815-817`,
`:834-836`) that item 11 converts from blanket-reject to exact accounting.

**Buildable friend-only BEFORE 8/9/10/12** (the slice to land at gate 17l's
red-test stage): `LAYOUT-LINEAR?`/`LAYOUT-LINEAR-COUNT` and the
`LIN-TYPE-COUNT*`/`LIN-TAINT-SCAN` layout awareness, unit-tested with synthetic
`TFAM-DECL`+`TFAM-SLOTS!`+`SUMV-ADD` registrations of a `TK-SUM` family whose
variant payload is `own` (mirroring `test/type-family-suite.f:200-205`), asserting
a hand-built layout row of that family cannot be copied/dropped. This proves the
counting core without waiting on constructors or `MATCH`. **The full `dup`/`MATCH`/
`KEEP`/deferred acceptance fixtures (`PLAN.md:750-753`) require 8/9/12.**

---

## 6. Trust surface — zero new trust rows achievable

Item 11 `Paths` = `src/core/checker.f`, `test/engine-suite.f`,
`docs/type-families.md` (`PLAN.md:743-744`).

- `checker.f` linear/layout logic is pure checked-checker code (unify guard, term
  walk, count). Adding `LAYOUT-LINEAR?`/`LAYOUT-LINEAR-COUNT` and the
  `LIN-TYPE-COUNT*` branch introduces **no `TRUST`/`TRUSTED:`/`set-check`**. These
  core files load in the checker bootstrap prefix and are not themselves checked
  user code, so no new *unchecked boundary* is created either.
- `test/engine-suite.f` already carries the linear-test harness with pre-existing
  trust rows used only to synthesize linear producers/consumers:
  `engine-suite.f:269` `TRUSTED: T-CHECK-PASSES`, `:893` `s" T-MAKE-OWN" s" --
  own" TRUST`, `:894` `s" T-FREE-OWN" s" own --" TRUST`. Item 11 **reuses** these
  (adds `T-MAKE-*`/`T-FREE-*` for a linear-layout family the same way) — no NEW
  trust *rows for ADT code itself*; these are test-fixture boundaries, exactly the
  audited pattern census-tfam-7 §5 / -8 §6 confirmed acceptable.
- `type-families.md` is documentation.

**Confirmation: item 11 can land with zero new trust rows**, satisfying the 17l
blocking rule "may not add `TRUST`, `TRUSTED:`, `set-check`, or `TRUSTED.md` rows"
(`PLAN.md:990-992`). No gap. (The `T-MAKE-*`/`T-FREE-*` fixtures are test producers
of the *abstract* linear resource, not generated ADT code, and add no `TRUSTED.md`
manifest rows.)

---

## 7. Acceptance criteria (`PLAN.md:750-753`) restated as a checklist

1. **`LAYOUT-LINEAR?` + `LAYOUT-LINEAR-COUNT` over expanded fields; any layout
   containing a linear payload is linear** (`PLAN.md:745-746`, spec `:1191-1193`).
   - New words in `src/core/checker.f` near `LIN-CON?`/`LIN-ROW-COUNT`
     (`:1044-1087`). `LAYOUT-LINEAR? ( type -- bool )` resolves the family (via
     the item-7 hidden-field identity + item-8 schema instantiation) and tests any
     variant payload `LIN-CON?`; `LAYOUT-LINEAR-COUNT ( row -- n )` = bundle-once
     scan replacing/wrapping `LIN-ROW-COUNT` (`:1080`).
2. **`result<own,n>` cannot be dropped or copied** (`PLAN.md:750`).
   - `LIN-TYPE-COUNT*` `T-PARAM` layout branch (`checker.f:1073-1075`) → count 1 →
     `LIN-CHECK` (`:1098`) rejects a width-aware `dup`/`drop` (item-12 ops).
     Fixtures in `test/engine-suite.f` beside `CBAD-OWN-DUP`/`CBAD-HDL-DUP`
     (`:898`,`:965`).
3. **`MATCH` consumes/refines linear payloads exactly once** (`PLAN.md:751`, spec
   `:1183-1186`).
   - Item-9 `CF-MATCH` frame + item-11 per-arm linear-count seeding/balance (§4).
     Fixtures flip `PLAN.md:666-668,679-680` reject cases to pass.
4. **Polymorphic laundering through `[: dup ;] execute`, `KEEP`, `BI`, deferred
   calls, or delayed type resolution rejects** (`PLAN.md:752-753`).
   - `LIN-EFF-PASS` (`checker.f:3108`, sites `:3136`,`:4482`) + `LIN-TAINT-SCAN`
     (`:1052`, `DO-TOK1:4998`), extended so a var binding a linear *layout* is
     treated like a linear con. Fixtures beside `CBAD-OWN-KEEP-LAUNDER`/
     `CBAD-OWN-BI-LAUNDER`/`CBAD-OWN-QUOT-DUP-FREE` (`engine-suite.f:924-927`).
5. **Non-linear ADTs retain scalar behavior** (`PLAN.md:753`).
   - `LAYOUT-LINEAR?` false ⇒ `LAYOUT-LINEAR-COUNT` contributes 0 ⇒ existing
     count path unchanged. Positive fixtures: a non-linear `result<n,n>` bundle
     copies/drops freely under item-12 ops.
6. **Gate 17l** (`PLAN.md:958-1027`): TDD red fixtures first; rebuild `bin/hb`;
   native self-refresh/fixpoint; no-binary Gforth bootstrap; size ratchet
   (`GE-CANDIDATE-SIZE-CHECK`); trust ratchet unchanged (§6);
   `filemap-lint`/`host-lint` green; `docs/type-families.md` filemap-covered.

---

## 8. Open risks / unknowns — each with a probe

- **R1 — Hidden-layout-field detection is undecided (blocks the count branch).**
  `LIN-TYPE-COUNT*`'s `T-PARAM` arm keys on `FIELD-PARAM?` = `FIELD-FAM`
  (`checker.f:535-538`). A layout slot's `PARAM>FAM` is the sum/enum/product
  family id, not `FIELD-FAM`, so item 11 needs a `LAYOUT-PARAM?`-style detector —
  whose exact shape depends on item 7's hidden-field identity choice
  (census-tfam-7 R2). *Probe (post-item-7):* register a `TK-SUM own`-payload
  family friend-only, expand a value, dump the top cell's `TAG`/`PARAM>FAM`, and
  confirm the detector distinguishes it from a `field<>` wrapper and from an
  ordinary user `family<...>`.
- **R2 — Count/movement boundary disagreement.** If item 12's width-aware `dup`
  moves `M+1` cells but `LAYOUT-LINEAR-COUNT` bounds the bundle at a different
  cell count, the +1/-1 accounting is wrong (PLAN risk `:754-755`). *Probe:* build
  a 2-slot sum, `dup` it under item-12 ops, assert the count delta is exactly +1
  bundle (reject), not +1 cell or +M.
- **R3 — `LAYOUT-LINEAR?` on a param payload needs instantiation.** For
  `result<ptr u8,own>` linearity comes from the *instantiated* arg, not the
  declaration (`SCH-PARAM`, `type-schema.f:90-92`). Without item 8's schema
  instantiation, `LAYOUT-LINEAR?` cannot see it. *Probe:* declare
  `SUMTYPE r<a> ... VARIANT some a END-VARIANT`, instantiate `r<own>` vs `r<n>`,
  assert `LAYOUT-LINEAR?` true vs false respectively.
- **R4 — SCALAR linear laundering through typed locals is UNCAUGHT today
  (Contradiction C1).** Reproduced on the current `bin/hb` (2026-07-04) via
  `CHECK-CANDIDATE!`: `( own -- own own ) {: x:own :} x x` **certifies (-1)**;
  `( own -- ) {: x:own :} x T-FREE-OWN x T-FREE-OWN` (double-free) **certifies
  (-1)**; `( own -- ) {: x:own :}` (drop via unreferenced local) **certifies
  (-1)**. The direct-stack equivalents reject (`CBAD-OWN-DUP` `:898`). **Static
  invariant that should hold:** a linear con bound to a local is consumed exactly
  once across the definition; a local reference that duplicates it, an
  unreferenced linear local, or a doubled destructor call must be rejected before
  runtime. **Where it should be enforced:** `LOC-BIND`/`LOC-REF?`
  (`checker.f:4294-4335`) — a local reference re-pushes the local tv WITHOUT a
  `LIN-CHECK`-covered step, so the count discipline never sees the copy. This is a
  **pre-existing scalar-linear hole**, not layout-specific, and item 11's "extend
  scalar `LIN-CON?` checks to layout values" (`PLAN.md:748-749`) presumes a sound
  scalar base that does not hold for locals. *Probe (regression to add):* the
  three strings above must flip from `-1` to `0`. **Owner decision needed:** fix
  as a prerequisite scalar-linear dot, or fold locals-linear-conservation into
  item 11/12 (item 12 already owns locals width-awareness, `PLAN.md:773-775`).
- **R5 — Layout typed-locals are unparseable today.** Probe `( hdl -- hdl hdl )
  {: h:hdl :} h h` rejects with `unknown type ':}' in signature` — the local-type
  parser (`LOCAL-TYPE` `checker.f:1868-1869`, `LOC-ANN`) reads a single token
  after `:`, so multi-token layout/record types cannot annotate a local. Item 12
  owns width-aware locals; item 11's linear-local fixtures depend on that landing.
  *Probe:* after item 12, `{: h:result<own,n> :}` must parse and bind as one
  bundle.
- **R6 — TWALK depth on the new schema descent.** §1i: the layout branch must
  bracket its descent with `TWALK-DEEPER`/`TWALK-SHALLOWER` or a recursive layout
  family overflows the stack instead of dying with code 76. *Probe:* a
  self-referential friend-registered sum's `LAYOUT-LINEAR?` must die with
  `"checker: term walk too deep"` (code 76), not SIGSEGV — pending item-16 policy
  rejection of recursive layouts.
- **R7 — `catch` has no `LIN-CHECK` (§1h).** `RSCATCH` (`checker.f:1208`) relies on
  row unification. A linear layout crossing a `catch` edge is item-12 metadata
  territory (`PLAN.md:833,837-839`); item 11 must add a fixture proving a linear
  bundle survives `catch` intact and cannot be stranded. *Probe:* `( result<own,n>
  -- result<own,n> ) [: ;] catch drop` must certify; a `catch` arm that drops the
  bundle must reject.

---

## Contradictions (PLAN/spec vs code)

- **C1 — "extend scalar `LIN-CON?` checks to layout values" presumes a sound
  scalar base that locals break.** `PLAN.md:748-749` frames item 11 as *extending*
  a working scalar linear discipline. Proven false for typed locals (R4): scalar
  `own` is copied/double-freed/dropped through `{: x:own :}` without rejection on
  the current binary. Neither item 11 nor item 12 explicitly assigns "linear
  conservation across local bind/ref"; item 12 assigns only *width*-awareness to
  locals (`PLAN.md:773-775`). Flagged for the orchestrator, not silently resolved:
  the locals-linear hole must be owned by a dot (prerequisite scalar fix or an
  item-11/12 line item) or the "layout locals are linear-safe" acceptance
  (implied by `PLAN.md:750-751` over `result<own,n>` in locals) cannot hold.
- **C2 — `Paths` omits `src/core/type-family.f` (and likely `type-schema.f`).**
  Item 11 `Paths` = `checker.f, engine-suite.f, type-families.md`
  (`PLAN.md:743-744`). But `LAYOUT-LINEAR?` must read family kind + variant
  payload schemas, reachable from checker.f only through a forward friend-xt cell
  installed in `type-family.f` (the pattern census-tfam-7 C2 already flagged for
  `TFAM-RESOLVE-XT`, `checker.f:357-358`, installed `type-family.f:652-653`). A new
  `TFAM-VARIANT-LINEAR?-XT`/schema-walk hook lives in `type-family.f`
  (loads AFTER checker.f). Recommend adding `src/core/type-family.f` (and
  `type-schema.f` if the schema resolver is shared) to item 11 Paths.
- **C3 — Dependency-order presumes 7-10 + 12 are built; the tree is at item ~4/7.**
  Order `... 10 -> 17k -> 11 -> 17l ...` (`PLAN.md:1031`) and `Depends on: items
  7-10 and 12` (`PLAN.md:757`) assume the layout ADT surface exists. §0 shows
  none of it does (7/8/12 open, 9/10 in-progress). Item 11's full acceptance
  (`PLAN.md:750-753`: copy/drop of `result<own,n>`, `MATCH` consumption,
  `KEEP`/`BI`/deferred laundering over layouts) is **not integration-testable
  today**; only the friend-only counting core (§5) is. Not a spec defect — a
  staging fact the implementer must respect (build the count core + red fixtures
  now; the flip-to-pass fixtures wait on 8/9/12), mirroring census-tfam-7 C1 and
  -8 C1.
- **C4 — Spec `LAYOUT-LINEAR-COUNT ( row -- n )` vs existing `LIN-ROW-COUNT (
  row -- n )`.** Spec `:1193` introduces `LAYOUT-LINEAR-COUNT` as if new, but
  `LIN-ROW-COUNT` (`checker.f:1080`) already IS the per-row linear counter. The
  correct reading (not a conflict once stated): `LAYOUT-LINEAR-COUNT` is
  `LIN-ROW-COUNT` **made bundle-granular**, not a parallel word — the implementer
  should extend the existing counter, not add a divergent second one, to keep the
  single `LIN-TOTAL` consumer graph (§3) intact. Noted so the two names are not
  implemented as two independent scans that can disagree.
