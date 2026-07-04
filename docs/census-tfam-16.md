# TFAM-16 Census — Layout Policies + Gate 17p

Dot: `habu-tfam-16-layout-a764d28c`. Scope: `PLAN.md` item 16
(`PLAN.md:934-956`) and its per-item proof gate **17p** (item 17,
`PLAN.md:958-1027`; sequence tail `... 15 -> 17o -> 16 -> 17p`,
`PLAN.md:1031` — item 16 is the LAST code item). Normative spec:
`docs/type-families.md` layout surface — §4 runtime-layout principle (`166-238`),
§6 registry `layout-policy` field (`294,314`), §9.2 `SUMTYPE` header (`494-503`),
§16 lowering (`970-1013`), §17 layout-aware ops + `?dup` policy gate
(`1041-1111`), §18 WIDTH (`1114-1129`), §20 renderer compaction (`1200-1227`),
§22 Runtime layout policies (`1293-1358`), §24 diagnostics (`1453-1474`),
§25.6 layout-op tests (`1624-1644`), §26 Phase-8 (`1779-1790`), §27 non-goals
(`1794-1805`), §28 (`1811-1836`). Token reservation: `PLAN.md:438` "item 16
reserves `POLICY`".

Every claim is `file:line` + a quoted definition/snippet. Symbol names are
authoritative (quoted). Paths absolute in the summary; relative here for density
(repo root `/Users/joel/Work/habu`).

Policy encoding (`type-family.f:24-29`): `TL-STACK-CELL-TAG 0  TL-PACKED-TAG 1
TL-NICHE 2  TL-BOXED 3  TL-CUSTOM 4  TL-MAX 4`. Family kinds
(`type-family.f:15-20`): `TK-CELL 0  TK-PRODUCT 1  TK-SUM 2  TK-ENUM 3
TK-EVIDENCE 4  TK-MAX 4`. `LAY-REC` columns (`type-family.f:452-458`):
`LAY.FAM LAY.POLICY LAY.SIZE LAY.ALIGN LAY.TAGW`.

---

## 0. STATE / PREREQUISITE ALERT (read first)

Item 16 declares `Depends on: items 9-15` (`PLAN.md:947`) and is the **terminal
code item** (`16 -> 17p`, `PLAN.md:1031`). **None of the ADT grammar/lowering
chain (items 5-15) is landed.** Evidence:

- Grammar/lowering words `SUMTYPE / TYPEFAMILY / PRODUCT / END-PRODUCT / MATCH /
  ENDMATCH / PUSH-LOGICAL / LAYOUT-PUSH-FIELDS` have **zero definitions** in
  `src/ lib/ tools/` (`rg -l '^: SUMTYPE|^: TYPEFAMILY|^: PRODUCT|^: END-PRODUCT|^: MATCH |^: ENDMATCH|PUSH-LOGICAL|LAYOUT-PUSH-FIELDS' src/ lib/ tools/`
  = 0 hits).
- `dot list` (2026-07): items **5,6,7,8,10,11,12,13,16** open (`o`); items
  **9,14,15** staged (`>`). Item 16 itself
  (`habu-tfam-16-layout-a764d28c`) is open. Tree sits at TFAM item-4 remainder.
- **`POLICY` is reserved-but-unbuilt and fail-closed.** Probe (read-only,
  `/private/tmp`):
  ```
  $ printf 'POLICY\n." reached" cr\n' | bin/hb   # from repo root
  E-UNDEFINED: POLICY      # hb exit 70; "reached" never prints
  ```
  Confirmed exit 70, empty stdout. The bare `POLICY` token binds to nothing;
  `SUMTYPE result 2 POLICY packed-tag` dies at `E-UNDEFINED: SUMTYPE` first
  (the header word does not exist either). So the whole policy grammar is
  fail-closed today; item 16 cannot land its `POLICY` parsing before item 6
  ships the `SUMTYPE` header it modifies.

Consequence: item 16 splits into **(A) the LAY-* / TF.LAYOUT registry
substrate** — already BUILT but exercised **only by unit tests** (§1), so a small
amount is checkable now — and **(B) the `POLICY` header grammar + policy
diagnostics + per-policy lowering** — the real deliverable, gated behind items
6-15. Unlike items 14/15 there is **no dependency-free early half**: the registry
is done, and everything else waits on the `SUMTYPE`/`PRODUCT` grammar.

---

## 1. The existing layout registry (what item 16 builds ON)

### 1a. Policy constants + the "what is a layout policy" definition
`type-family.f:22-29`:
```
\ --- layout policy (physical representation). Default is the universal
\ M-payload-cells + 1-tag-cell stack representation (docs §22.1).
0 constant TL-STACK-CELL-TAG
1 constant TL-PACKED-TAG
2 constant TL-NICHE
3 constant TL-BOXED
4 constant TL-CUSTOM
4 constant TL-MAX
```
The source comment fixes the semantics: **a "layout policy" is a per-family
physical-representation choice** (not per-use-site). Spec agrees — §6 lists it as
a single TFAM record column, `type-families.md:294`
"`layout-policy       stack-cell-tag | packed-tag | niche | boxed | custom`",
and it is one of the **minimum v1 fields** (`:314` `layout-policy`). §22 is the
per-policy meaning:
- **§22.1 default `stack-cell-tag`** (`type-families.md:1295-1303`): "Universal
  v1 representation … `M payload cells + 1 tag cell` … the default for all
  sums/enums."
- **§22.2 packed** (`:1305-1316`): "**Keep stack representation as cells**, but
  allow a memory ABI descriptor: `tag-byte-width = u8|u16|u32|cell` /
  `payload-offsets` / `alignment` / `size`. This matters later for arrays of
  ADTs, GPU buffers, and ABI-stable structs." — i.e. packed is a *memory-ABI*
  (packing/alignment/width) overlay, NOT an on-stack change.
- **§22.3 niche** (`:1318-1337`): `SUMTYPE option 1 POLICY niche-null` → "one
  cell"; `none`=null, `some`=non-null. "Do not make this implicit."
- **§22.4 boxed** (`:1339-1358`): `SUMTYPE tree 1 POLICY boxed` → stack rep
  `ptr tree-box<a>`; "Use this for recursive or large ADTs. **Do not start with
  boxed layout.**"

So policy binds **per-family**, is chosen at declaration via a `POLICY <name>`
header modifier (§22.3/§22.4 show it on the `SUMTYPE` header line, after arity),
and governs packing/alignment/tag-width/boxing — never a per-use-site decision.

### 1b. The per-family policy field on the TFAM record (TF.LAYOUT)
`type-family.f:157` `CELL +FIELD TF.LAYOUT` (one column of `TF-REC`,
`:148-167`). Reader `type-family.f:199`
`: TFAM-LAYOUT-POLICY@ ( id -- policy ) TF-REC@ TF.LAYOUT @ ;`. Friend-only
mutator + its ONLY validation, `type-family.f:222-224`:
```
: TFAM-LAYOUT! ( id policy -- ) {: id:n p:n :}
   p 0 < p TL-MAX > or IF E-TFAM-KIND throw THEN
   p id TF-REC@ TF.LAYOUT ! ;
```
Default stamped at declaration, `type-family.f:310`
`TL-STACK-CELL-TAG r TF.LAYOUT !` (inside `TFAM-DECL`, `:293-318`). So **every
family already defaults to `stack-cell-tag`** — the "missing policy defaults to
`stack-cell-tag`" acceptance (`PLAN.md:940`) is *already the registry
invariant*, proven by `test/type-family-suite.f:97`
`FID @ TFAM-LAYOUT-POLICY@ TL-STACK-CELL-TAG T=`.

### 1c. TF.SLOTS (M payload cells) + TF.TAGW (tag width) — the width inputs
`type-family.f:159` `CELL +FIELD TF.SLOTS` (= M, max payload cells),
`:163` `CELL +FIELD TF.TAGW` (tag width). Readers `:200`
`: TFAM-SLOTS@ ( id -- n )`, `:205` `: TFAM-TAGW@ ( id -- n )`; mutators `:225`
`: TFAM-SLOTS! ( id n -- )`, `:230` `: TFAM-TAGW! ( id w -- )`. Defaults at decl:
`:311` `0 r TF.SLOTS !`, `:314` `TAGW-CELL r TF.TAGW !` where
`:38` `CELL constant TAGW-CELL \ default tag width: one stack cell`. These are
the spec §18 WIDTH inputs (`type-families.md:1126-1128`):
`WIDTH(sum) = max variant payload widths + tag width`, `WIDTH(enum) = tag width`,
`WIDTH(boxed<...>) = 1`. A **packed** policy narrows `TF.TAGW`/`LAY.TAGW`
(u8/u16/u32); a **boxed** policy forces physical width to 1 (`:1128`). The tag
width is thus the exact field a policy rewrites.

### 1d. The LAY-* logical-layout registry (physical size/align/tagw per family)
Declared `type-family.f:449-503`. Record `:452-458`:
```
BEGIN-STRUCTURE LAY-REC
   CELL +FIELD LAY.FAM
   CELL +FIELD LAY.POLICY
   CELL +FIELD LAY.SIZE
   CELL +FIELD LAY.ALIGN
   CELL +FIELD LAY.TAGW
END-STRUCTURE
```
Readers `:480-485`: `LAY-FAM@ LAY-POLICY@ LAY-SIZE@ LAY-ALIGN@ LAY-TAGW@ LAY-N@`.
Lookup `type-family.f:487-493` `: LAY-FIND ( fam -- id true | false )` — **keyed
by family-id, one layout row per family** (linear scan). Writer
`type-family.f:494-503`:
```
: LAY-ADD ( fam policy size align tagw -- id ) {: fam:n p:n sz:n al:n tw:n :}
   p 0 < p TL-MAX > or IF E-TFAM-KIND throw THEN
   fam LAY-FIND IF drop E-TFAM-DUP throw THEN drop
   LAY-ENSURE …  fam r LAY.FAM !   p r LAY.POLICY !   sz r LAY.SIZE !
   al r LAY.ALIGN !   tw r LAY.TAGW !   id ;
```
This is precisely the **§22.2 packed ABI descriptor** store (`size`/`alignment`/
`tag-byte-width`, `type-families.md:1310-1313`). It is BUILT and rollback/persist
wired (§1e) but has **zero production writers** — the only `LAY-ADD` callers
tree-wide are `test/type-family-suite.f:243-250` and
`test/type-family-rollback-suite.f:69` (§1f). No grammar populates it.

### 1e. Rollback + snapshot persist already cover the policy/layout state
- Rollback frame saves the layout high-water: `type-family.f:531`
  `CELL +FIELD TFRB.LAYN`; saved `:558` `LAY-N @ r TFRB.LAYN !`; restored `:568`
  `r TFRB.LAYN @ LAY-N !`. So a rejected declaration that added a `LAY-ADD` row
  retires it (counter restore = entry retirement, `:518-523`).
- Snapshot persist bakes the layout arena into image DATA: `type-family.f:599`
  `LAY-A-P   LAY-A-BOOT   LAY-CAP-V @ LAY-REC *    REG-PERSIST-BUF drop` (inside
  `TFAM-SNAPSHOT-PERSIST`, `:594-602`). `TF.LAYOUT` rides the TFAM record
  persist (`:595`). Reset zeroes it: `:510` `0 LAY-N !`.
- **`TL-CUSTOM(4)` is stored but never lowered** — `LAY-ADD`/`TFAM-LAYOUT!`
  accept `0..TL-MAX` inclusive (`:495,223`), and `TL-MAX = TL-CUSTOM = 4`
  (`:28-29`), so the registry *admits* `custom` today even though §27 lists
  "user-defined custom layout code" as a **non-goal for v1**
  (`type-families.md:1803`). This over-permission is a latent gap (§Contradictions C3).

### 1f. Every existing exerciser of the policy/layout surface (test-only)
- **`test/type-family-suite.f`** (registered in the engine gate,
  `test/gate-engine-lib.f:349` `s" test/type-family-suite.f" GE-SRC-FILE+`):
  - default policy `:97` `TFAM-LAYOUT-POLICY@ TL-STACK-CELL-TAG T=`.
  - policy mutate `:165` `FID @ TL-PACKED-TAG TFAM-LAYOUT!   FID @
    TFAM-LAYOUT-POLICY@ TL-PACKED-TAG T=`.
  - LAY registry `:243-250` — all 5 policies exercised: `TL-STACK-CELL-TAG`
    (`:243`, size 16), `TL-PACKED-TAG` (`:244`, size 24 align 8 tagw 4),
    `TL-BOXED` (`:246`), `TL-CUSTOM` (`:247`, "5 layouts > seed cap 4 -> LAY
    grew"); `LAY-FIND` (`:248-249`), dup-fam reject `:250`
    `' LAY-ADD catch … TC @ E-TFAM-DUP T=`.
  - persist round-trip `:282` `FID @ LAY-FIND … LAY-SIZE@ 16 T=`.
- **`test/type-family-rollback-suite.f`** — LAY rollback proof: `:57`
  `LAY-N@ P-LAY !`, `:69` `FOUNDF @ TL-BOXED 8 8 8 LAY-ADD drop`, `:80`
  `LAY-N@ P-LAY @ T=` (added-then-rolled-back layout leaves LAY-N unchanged).

**Finding: the policy/layout substrate is complete and tested at the unit level;
item 16 supplies the missing GRAMMAR (`POLICY` header → `TFAM-LAYOUT!`/`LAY-ADD`)
+ the per-policy diagnostics and lowering, none of which exists.**

---

## 2. Where widths/layouts are consumed — every place a policy binds

### 2a. Checker width table (CT-WIDTH) — the per-type-class cell width
`checker.f:643` `create CT-WIDTH-BOOT CT-CAP-INIT cells allot`; register at
`:745` `width code cells CT-WIDTH + !`; read `:786-787`
`: CT-WIDTH@ ( n -- n )  cells CT-WIDTH + @ ;`; coercion width guard
`:820,825` (`got CT-WIDTH@ want CT-WIDTH@ <= …`). This is the physical cell-width
each checker type class occupies. A layout family's physical width (M+tag, or 1
under boxed) is what item-7/12 must thread through here; **item 16's policy is the
value that decides that width** (default M+tag vs boxed 1 vs niche 1). Today
`SIG-FAM?` (`checker.f:1885-1886` `s" " 2swap TFAM-RESOLVE*`) is **kind-blind**:
it resolves a family token to a family-id and treats it as **one cell**,
regardless of `TF.LAYOUT`/`TFAM-LAYOUT?`. So no width or policy is consumed at
all yet — the entire consumption path is item-7/12 work that item 16 parameterizes.

### 2b. Item 12 — layout-aware stack ops (the primary consumer of width)
`PLAN.md:760-817`; spec §17 (`type-families.md:1041-1111`) and §25.6
(`:1624-1644`). `dup`/`drop`/`swap`/`over`/`nip`/`rot`/`2dup`/… must operate on
**logical bundles of physical width W(policy)**. The policy directly changes W:
`stack-cell-tag` → W = M+tag (multi-cell), `niche-null`/`boxed` → W = 1
(`type-families.md:1128,1332,1353`). **`?dup` is policy-gated**:
`type-families.md:1103-1105` "`?dup` is not a generic layout operation … It must
reject layout values **unless a family policy defines a checked truthiness/niche
representation**"; restated `:1637-1638` and `PLAN.md:810-814`. So item 16's
`niche-null` is the exact policy that *unblocks* `?dup`/`if` on that family;
until a policy declares truthiness, item 12 rejects. **This is the one place
where a policy is not just physical-ABI but changes CHECKER acceptance.**

### 2c. Item 10 — native/Gforth lowering (constructor + MATCH tag emit)
`PLAN.md:690-741`; spec §16 (`type-families.md:970-1013`). Constructors emit
"push zero padding M-p times / push tag k" (`:978-988`); MATCH emits
"peek tag / cmp / branch" (`:990-1011`). The **tag width** the emit uses is
`TF.TAGW`/`LAY.TAGW` — a packed policy narrows it to u8/u16/u32
(`type-families.md:1310`), and boxed replaces the whole sequence with a pointer
deref (`:1350-1353`). PLAN Risk (`PLAN.md:944-945`): "packed/niche/boxed policies
change physical layout and can break compiler assumptions **if exposed before
lowering support**" — i.e. a policy may not be publicly selectable until item 10
can lower it. This is why acceptance stages them "as separate checked extensions
… before exposing them publicly" (`PLAN.md:938-939`).

### 2d. PTX IR value shapes — the campaign's PRODUCT-unification target
`lib/ptx/ir.f:18` `VALUE-RECORD ptxir-node op n a n b n val n live n
END-VALUE-RECORD` — a 5-cell by-value bundle. Physical node storage is a
**separate** array: `:20-28` `BEGIN-STRUCTURE PTXIR-REC … END-STRUCTURE` +
`create PTXIR-NODES PTXIR-MAX PTXIR-REC * allot`. Under PRODUCT-unification
(item 15), `ptxir-node` becomes a `TK-PRODUCT` family whose fields are cells;
its policy is `stack-cell-tag` (the default — products have no tag, `WIDTH =
Σ field widths`, `type-families.md:1125`), so **item 16 leaves the IR bundle at
the default policy**. A packed policy on such a product is what the follow-on
"array-of-ADT container over the packed ABI descriptor" dot
(`habu-checker-capability-typed-a480c423`, `PLAN.md:949-956`) would use for a
`PTXIR-NODES`-style contiguous store — i.e. `LAY.SIZE`/`LAY.ALIGN` become the
array stride. The kernel emitters (`lib/ptx/cg*.f`, `lib/ptx/tile*.f`) operate on
`TK-CELL` width-1 GPU-context families (`tile`/`acc`/`gridctx`/…, registered
`type-family.f:630-643`), which are **not layout families** — policy never
touches them (§3).

### 2e. FFI boundary — cell-only today; packed ABI descriptor is the future seam
`lib/ffi.f` types foreign args through a per-arg **cell tag table**
(`lib/ffi.f` `create FDEF-TAG FFI-MAX-ARGS cells allot`, tags `FDEF-N/PTR/NOM/
VOID`); `lib/ffi-abi.f:5-10,56` passes "**prepacked cells**" (x0-x8/d0-d7 +
caller-packed stack spill). There is **no path to pass a layout-family value
across FFI** — a multi-cell ADT would need the §22.2 packed descriptor
(`LAY.SIZE`/`LAY.ALIGN`) to marshal as an ABI-stable struct. So FFI is a place a
**packed** policy *would* bind, but only once the follow-on typed-buffer
capability lands; item 16 itself adds no FFI coupling.

### 2f. Renderer compaction — policy-independent, but width-aware
Spec §20 (`type-families.md:1200-1227`): compaction scans a row and renders
`family<args>` when consecutive cells match a registered layout pattern. A boxed
family is one cell, so its compaction differs from a stack-cell-tag family
(M+tag cells). "This is rendering only. Checker correctness should not depend on
renderer compaction" (`:1227`). Item 16 must ensure the renderer reads
`TFAM-LAYOUT-POLICY@`/`LAY-*` to know how many physical cells a family occupies.

---

## 3. Campaign-goal implication: policies must not leak cost onto the hot path

The goal "PRODUCT-unified value records and PTX IR" (`PLAN.md:12` region) plus
the maki Model-CAD workload implies two regimes:

1. **Host cell rows.** Products/sums on the host stack stay physical cells under
   the default `stack-cell-tag` policy — "no heap; no GC; no fat pointer; no
   hidden runtime metadata; values remain stack/register friendly"
   (`type-families.md:230-238`). The LAY-* registry is **compile-time-only
   metadata** in the checker arena — a `LAY-ADD` row is integers/offsets, baked
   into image DATA at snapshot (`type-family.f:599`), consumed at check/lowering
   time. It produces **zero runtime allocation** and adds no per-value cost. This
   satisfies the project **zero-alloc** rule and keeps the checker's hot compile
   loop clean.
2. **GPU tile/register-resident shapes.** The GPU workhorses (`tile`/`acc`/
   `gridctx`/`mmctx`/…, `type-family.f:630-643`) are `TK-CELL` **width-1**
   families — `TFAM-LAYOUT?` is FALSE for them (`type-family.f:214,218`), so
   **no policy machinery touches them**. A tile stays one cell; policy dispatch
   never enters the kernel emit path. This is the load-bearing "must not leak
   cost onto the hot path" guarantee: policy is a *layout-family-only* concept,
   and the GPU hot families are deliberately cell families.

**<192KB L1I constraint:** the policy dispatch must not balloon the hot lowering
words. The correct shape is a small `TFAM-LAYOUT-POLICY@`-keyed branch at
*declaration/lowering* time that resolves to already-specialized emit words —
not a per-instruction policy check inside `dup`/`MATCH` codegen. packed's ABI
descriptor lives in the LAY-* arena (data), not in instruction cache.

**Where the abstraction must not leak:** `stack-cell-tag` and `packed-tag` both
"keep stack representation as cells" (`type-families.md:1307`) — packed only adds
a *memory-ABI descriptor* for arrays/buffers. Only **boxed** introduces a runtime
heap pointer (`:1350-1353`), and it is explicitly deferred ("Do not start with
boxed layout", `:1358`; §27 non-goal-adjacent). So item 16's *shippable* default
(stack-cell-tag) and its first extension (packed-tag, the maki prerequisite,
`PLAN.md:955-956`) both cost nothing on the on-stack hot path; boxed's heap cost
is opt-in and gated.

---

## 4. Dependencies — artifacts item 16 consumes; buildable early?

Item 16 `Depends on: items 9-15` (`PLAN.md:947`); transitively 5-8/10-13. All
UNBUILT (§0). Per-item artifacts item 16 needs:

- **Item 6 (`SUMTYPE`/`TYPEFAMILY` grammar + header parsing, UNBUILT):** the
  `POLICY <name>` modifier is a **`SUMTYPE`/`PRODUCT` header extension** (spec
  shows it inline: `SUMTYPE option 1 POLICY niche-null`,
  `type-families.md:1323`; `SUMTYPE tree 1 POLICY boxed`, `:1344`). Item 16
  cannot parse `POLICY` until item 6 owns the header line and reserves the sibling
  tokens (`PLAN.md:434`). Item 16 reserves `POLICY` itself (`PLAN.md:438`).
- **Item 7 (hidden fields / `LAYOUT-PUSH-FIELDS`, UNBUILT):** the physical-field
  expansion whose **width** a policy sets. `TF.SLOTS` (M) is set by item 7's
  layout computation; item 16's policy chooses how M+tag maps to physical cells.
- **Item 8 (generated constructors, UNBUILT):** constructor emit consumes the
  policy's tag width / padding (§16 `type-families.md:978-988`). Acceptance
  "each implemented policy has **constructor** … tests" (`PLAN.md:942`) needs
  item 8's generator to be policy-parameterized.
- **Item 9 (`MATCH`, UNBUILT):** MATCH tag-compare consumes tag width; "each
  implemented policy has … **match** … tests" (`PLAN.md:942`).
- **Item 10 (native/Gforth lowering, UNBUILT):** the hard gate for exposing any
  non-default policy — Risk `PLAN.md:944-945`. niche/boxed change emit; packed
  changes tag byte width. Acceptance "**invalid-tag** tests" (`PLAN.md:943`) is
  item 10's bad-tag runtime death (spec §25.5 `:1600-1622`) per policy.
- **Item 12 (layout-aware stack ops, UNBUILT):** "each implemented policy has …
  **stack-op** … tests" (`PLAN.md:942`); `?dup` truthiness is unlocked *by* the
  niche policy (§2b). This is the tightest coupling: item 16's niche policy and
  item 12's `?dup` rule are two halves of one rule.
- **Items 14/15 (enum/product families, staged):** provide the concrete
  layout-bearing families a policy attaches to. Item 15's PRODUCT is the default
  `stack-cell-tag` case (no tag); item 14's ENUM is `WIDTH = tag width` (`:1127`).

**Buildable early (in this tree, dependency-light):**
- The registry substrate is DONE (§1) — but nothing new is buildable against it
  without the grammar. The one genuinely-early act is **choosing the reject code
  + diagnostic string** for invalid/unsupported/recursive policy (§6, §Contra C2)
  and adding the negative unit fixture to `test/type-family-suite.f` (which
  already drives `LAY-ADD`/`TFAM-LAYOUT!` directly, `:165,243-250`) — a checked
  registry-level test that does not need the grammar.

**Waits on 6-15:** the `POLICY` header word, per-policy constructor/match/stack-op
lowering, `?dup`/niche truthiness, packed ABI marshalling, and every fixture that
needs a real declared family with a non-default policy.

---

## 5. Trust surface — zero new trust rows achievable

**Yes, zero new trust rows.** Evidence over item-16 touched surfaces:
- `src/core/type-family.f` (the registry): whole-file read — **no `TRUST`,
  `TRUSTED:`, or `set-check`** anywhere (`rg 'TRUST|set-check'
  src/core/type-family.f` = 0). `LAY-ADD`/`TFAM-LAYOUT!` are ordinary checked
  words that `throw` typed reject codes (`type-family.f:223,495`). Adding
  `POLICY` parsing over this substrate adds no trust.
- Item 16 Paths (`PLAN.md:935`) = `docs/type-families.md`, `src/core/checker.f`.
  The `POLICY` grammar lands via the same structured declaration path as item 6's
  `SUMTYPE` header — **not** an `evaluate`-based path — so no `TRUSTED.md` row.
- **Gate-17p ratchet (`PLAN.md:989-992`):** "The type-family/ADT campaign may not
  add `TRUST`, `TRUSTED:`, `set-check`, or `TRUSTED.md` rows unless a separate
  non-ADT dot is approved." Policy metadata is pure checker data; no runtime
  trust needed.

**Precise gap to watch:** the §25.5 invalid-tag runtime test per policy
(`type-families.md:1600-1607`) must "use … only the existing image-writer trust
rows … **Do not introduce any new ADT `TRUST`/`TRUSTED:`/`set-check`/`TRUSTED.md`
row** to forge payload slots plus an invalid tag." A boxed-policy bad-tag test
that seeds a raw pointer must ride existing image-writer rows, not a new one.
**Verdict: zero new trust rows is achievable for item 16.** No gap.

---

## 6. PLAN item 16 acceptance as a checklist (files/words per criterion)

From `PLAN.md:940-943`. Paths declared (`PLAN.md:935`):
`docs/type-families.md`, `src/core/checker.f`.

1. **"missing policy defaults to `stack-cell-tag`"** → already the registry
   invariant: `TFAM-DECL` stamps `TL-STACK-CELL-TAG r TF.LAYOUT !`
   (`type-family.f:310`); proven `test/type-family-suite.f:97`. The grammar must
   preserve this when no `POLICY` clause is present — i.e. the `SUMTYPE`/`PRODUCT`
   header parser leaves `TF.LAYOUT` at its `TFAM-DECL` default.
2. **"invalid policies reject"** → parse `POLICY <name>` → map name to a `TL-*`
   constant → `TFAM-LAYOUT!` (`type-family.f:222`, throws on `p>TL-MAX`).
   **Gap:** the reject code is `E-TFAM-KIND` (7105 "unknown kind",
   `type-family.f:48,223,495`), which is **the wrong diagnostic** for a policy
   error — needs a dedicated `E-TFAM-POLICY`-class code + the documented string
   (§Contra C2). Negative fixture in `test/type-family-suite.f`.
3. **"recursive or unsupported layouts reject with the documented diagnostic"**
   → spec §24 `type-families.md:1473` "invalid layout policy for recursive sum";
   §22.4 boxed is the *only* policy that permits recursion (`:1339-1358`), so a
   non-boxed recursive family must reject. **Gap:** there is **no recursion/cycle
   detection** for layout families today (`rg` for cycle guards finds only
   `render.f` + term-walker guards; the schema `SC-LAYOUT` tag `:436` has no
   cycle check). Item 16 (or its item-6/7 substrate) must add layout-recursion
   detection; "unsupported policy" (e.g. `custom`, §27 non-goal `:1803`) must
   also reject at the grammar even though the registry stores it (§1e).
4. **"each implemented policy has constructor, match, stack-op, and invalid-tag
   tests"** → per-policy test matrix: constructors (item 8, spec §16
   `:972-988`), `MATCH` (item 9, §16 `:990-1011`), stack ops (item 12, §25.6
   `:1624-1644`), invalid-tag runtime death (item 10, §25.5 `:1600-1622`).
   `stack-cell-tag` is the default and gets the full matrix first; `packed-tag`,
   `niche-null`, `boxed` are "**separate checked extensions with layout tests
   before exposing them publicly**" (`PLAN.md:938-939`). Owning suites:
   `test/type-family-suite.f` (registry, `gate-engine-lib.f:349`) +
   `test/engine-suite.f` (checked fixtures).
5. **Work "Ship `stack-cell-tag` as the required default"** (`PLAN.md:936`) →
   deliver the grammar + default; **do NOT expose packed/niche/boxed publicly**
   until item 10 lowering supports them (Risk `:944-945`, spec Phase-8
   `:1779-1790` "Keep default as `stack-cell-tag`").

### Gate 17p checklist (item 17 applied to item 16, `PLAN.md:958-1027`)
- TDD red policy fixtures before impl; rebuild `bin/hb`; native
  self-refresh/fixpoint; focused checker/engine suites (`:968-970,995-997`).
- **Trust ratchet unchanged** (§5): exact `TRUSTED.md` count + inventory
  identical before/after (`:989-992`).
- `tools/filemap-lint.f` covers `docs/type-families.md` — **already satisfied**:
  `FILEMAP.md:22` registers it; `tools/filemap-lint.f:231`
  `s" docs/type-families.md" FM-REQ`. `src/core/type-family.f` at `FILEMAP.md:44`.
- **No new core file** is implied (Paths are `checker.f` + doc), so `srclist.f`/
  `run-files.f` result-cache/`hb-build-lib.f`/prefix rows need edits **only if**
  a new `src/core/policy.f`-style file is added (cf. `enums.f` precedent, item
  14). If the grammar lands in an item-6 grammar file, that file's closure edits
  ride item 6, not 16.
- `GE-CANDIDATE-SIZE-CHECK` vs `test/gate-build-size.f` (`:997-999`) — policy
  parsing is small; watch the per-policy emit expansion (packed/niche/boxed) for
  L1I/size growth. No-binary Gforth bootstrap to fixpoint (`:995-996`).
- Master advances only on exact-tree green (`:1020-1021`).

---

## 7. Open risks / unknowns — each with a probe

- **R1 — Whole chain (5-15) unbuilt; `POLICY` fail-closed (§0).** Item 16 cannot
  land any grammar first. *Probe (done §0):* `printf 'POLICY …' | bin/hb` →
  `E-UNDEFINED: POLICY` exit 70. Re-run after item 6 lands to confirm `POLICY`
  parses only inside a `SUMTYPE`/`PRODUCT` header, and rejects standalone.
- **R2 — No dedicated policy reject code / diagnostic (§6.2, §Contra C2).**
  `LAY-ADD`/`TFAM-LAYOUT!` throw `E-TFAM-KIND` (7105 "unknown kind") for a bad
  policy, but spec wants "invalid layout policy…" (`type-families.md:1473`).
  *Probe:* `rg 'E-TFAM-POLICY|E-LAYOUT|invalid.*policy' src/` = 0 today. Add a
  distinct code (e.g. `7107 constant E-TFAM-POLICY`) + string; assert a bad
  `POLICY foo` reports it, not "unknown kind".
- **R3 — No layout-recursion detection (§6.3, §Contra C4).** "recursive …
  layouts reject" has no substrate. *Probe:* `rg 'SC-LAYOUT|cycle|recursi'
  src/core/*.f` → only render/term-walker guards; schema `SC-LAYOUT` tag has no
  cycle check. Declare `SUMTYPE tree 1 … VARIANT node tree<a> …` under a
  non-boxed policy and confirm it must reject (needs new detection).
- **R4 — `TL-CUSTOM` stored but non-goal (§1e, §Contra C3).** Registry admits
  `custom(4)` (`type-family.f:28-29,223,495`) though §27 forbids it
  (`type-families.md:1803`). *Probe:* the grammar must map no user token to
  `TL-CUSTOM`; assert `POLICY custom` rejects as "unsupported policy" even though
  `LAY-ADD … TL-CUSTOM` (registry) succeeds in the unit test
  (`type-family-suite.f:247`). The registry validation is intentionally broader
  than the grammar surface — document that seam.
- **R5 — niche/boxed exposed before lowering breaks codegen (Risk
  `PLAN.md:944-945`).** *Probe:* after item 10, declare `SUMTYPE option 1 POLICY
  niche-null` and run the §25.5 bad-tag test on native + Gforth output; a missing
  lowering path surfaces as a codegen assertion. Keep niche/boxed grammar-gated
  ("not exposed publicly") until this passes.
- **R6 — `?dup`/niche coupling (§2b).** niche policy is what unblocks `?dup`/`if`
  on a layout family (`type-families.md:1103-1105`). *Probe:* before a truthiness
  policy, `: T ( option<a> -- ) ?dup … ;` must reject (item 12); after `POLICY
  niche-null`, the same must pass. The two rules must land together or `?dup` is
  either universally rejected or unsoundly accepted.
- **R7 — PTX IR / GPU families untouched by policy (§2d, §3).** *Probe:* confirm
  `tile`/`acc`/`gridctx` stay `TFAM-CELL?` (`type-family.f:214`) so
  `TFAM-LAYOUT?` is FALSE — no policy dispatch enters the kernel emit path. If a
  GPU family ever became a layout family, re-audit the hot path for policy cost.
- **R8 — Packed ABI descriptor has no consumer yet (§2e).** `LAY.SIZE/ALIGN/TAGW`
  are stored but nothing reads them for marshalling. *Probe:* the first real
  consumer is the follow-on dot `habu-checker-capability-typed-a480c423`
  (`PLAN.md:949-956`), NOT item 16. Item 16 ships the descriptor + validation;
  don't over-build a buffer API into this dot.

---

## Contradictions (PLAN / spec vs code)

- **C1 — Item 16 Paths omit `src/core/type-family.f`.** Paths are
  `docs/type-families.md, src/core/checker.f` (`PLAN.md:935`), but the entire
  policy substrate the grammar drives — `TF.LAYOUT`/`TFAM-LAYOUT!`
  (`type-family.f:157,222`), the `TL-*` constants (`:24-29`), and the `LAY-*`
  registry (`:452-503`) — lives in `type-family.f`. The `POLICY` **header token**
  is parsed in whatever file owns the `SUMTYPE`/`PRODUCT` grammar (item 6/15,
  likely a new grammar file), not `checker.f`. Like items 14/15, the declared
  Paths under-specify; expect edits to `type-family.f` (policy code/diagnostics)
  and the item-6 grammar file, plus `test/type-family-suite.f` and
  `test/engine-suite.f` fixtures.
- **C2 — Policy errors reuse `E-TFAM-KIND` ("unknown kind"), not a policy
  diagnostic.** `LAY-ADD` (`type-family.f:495`) and `TFAM-LAYOUT!` (`:223`) both
  `E-TFAM-KIND throw` on an out-of-range policy — code 7105 "unknown kind"
  (`:48`). But acceptance wants "invalid policies reject … with the documented
  diagnostic" (`PLAN.md:940-942`) and spec §24 pins "invalid layout policy for
  recursive sum" (`type-families.md:1473`). A `POLICY` error surfacing as
  "unknown kind" is a wrong-diagnostic bug; item 16 must add a distinct
  policy-reject code + message.
- **C3 — Registry admits `TL-CUSTOM` though it is a v1 non-goal.**
  `TL-MAX = TL-CUSTOM = 4` (`type-family.f:28-29`), so `LAY-ADD`/`TFAM-LAYOUT!`
  accept `custom` (`:223,495`), and `test/type-family-suite.f:247` stores it. Yet
  §27 lists "user-defined custom layout code" as a **non-goal for v1**
  (`type-families.md:1803`) and Phase-8 ships only stack-cell-tag/packed/niche/
  boxed (`:1784-1787`). The registry's validation range (`0..TL-MAX`) is broader
  than the grammar should accept; item 16 must reject `POLICY custom` at the
  grammar even though the registry tolerates it.
- **C4 — "recursive … layouts reject" has no detection substrate.** Acceptance
  (`PLAN.md:941-942`) and §24 (`type-families.md:1473`) require rejecting a
  recursive family under a non-boxed policy, but no layout-cycle detection exists
  (`rg 'SC-LAYOUT|cycle|recursi' src/core/*.f` → only render/term-walker guards).
  §22.4 makes boxed the *only* recursion-legal policy (`:1339-1358`). The
  detection is unbuilt — either item 7 (hidden-field expansion) or item 16 must
  add it; it is not in the current tree.
- **C5 — `POLICY` grammar position is implied, not specified in §9.** §9.2's
  `SUMTYPE` syntax (`type-families.md:498-503`) shows no `POLICY` clause; the
  keyword only appears in §22.3/§22.4 examples inline after arity
  (`SUMTYPE option 1 POLICY niche-null`, `:1323`). The exact header slot
  (before/after arity, before the first `VARIANT`) is under-specified; the spec
  §9 defining-words section should show the `POLICY` clause explicitly so item 6
  and item 16 agree on parse order.
- **C6 — "Depends on: items 9-15" understates the grammar dependency.** Item 16
  needs item **6**'s `SUMTYPE`/`PRODUCT` header to hang `POLICY` on (spec inline
  examples `:1323,1344`), but 6 is not in the `9-15` list (`PLAN.md:947`). The
  order (`PLAN.md:1031`) places 6 far upstream so it is transitively implied, but
  a literal reader of "9-15" would miss that the header grammar (item 6) is the
  direct host of the `POLICY` token item 16 reserves (`PLAN.md:438`).
