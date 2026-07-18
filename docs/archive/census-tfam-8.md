# TFAM-8 Generated-Constructors Census — dot `habu-tfam-8-generated-f89a7ae9`

Scope: PLAN.md item 8 "Generate constructors without emitted trust"
(`PLAN.md:571-644`) and its per-item proof gate `17i` (item 17,
`PLAN.md:958-1027`; gate slot `12 -> 17h -> 8 -> 17i -> 9` at `PLAN.md:1031`).
Normative spec: `docs/type-families.md` §5 (`242-276`), §7 (`344-421`), §12
(`645-784`). Every claim below is `file:line` + quoted snippet. Symbol names are
authoritative.

---

## 0. STATE / PREREQUISITE ALERT (read first)

Item 8 declares `Depends on: items 7 and 12` (`PLAN.md:643`) and the dependency
chain routes `... 6 -> 17f -> 7 -> 17g -> 12 -> 17h -> 8` (`PLAN.md:1031`).
**Those prerequisites are NOT landed in this tree.** VCS + `dot list` evidence:

- Latest TFAM commit is item 4: `ykmoqtwr TFAM 4: registry-driven params +
  family-id terms`, `yszlypno Fix nested-param instantiation cycle crash`,
  `zpmlnmot Record TFAM 4 deferrals as remainder dot` (jj log).
- Open dots (`o` = open): `habu-tfam-5-ordered`, `habu-tfam-6-typefamily`,
  `habu-tfam-7-hidden`, `habu-tfam-12-layout`, **`habu-tfam-8-generated`**,
  `habu-tfam-4-remainder` (SC-QUOT/uncapped arity/package-aware SIG still open).
- Grammar words `SUMTYPE / TYPEFAMILY / VARIANT / ;VARIANT / ;SUMTYPE`
  and `PUSH-LOGICAL / LAYOUT-PUSH-FIELDS` have **zero definitions** anywhere in
  `src/ lib/ tools/` (`rg '^: SUMTYPE|^: VARIANT|^: TYPEFAMILY|PUSH-LOGICAL'` =
  0 hits). Confirmed at `src/core/checker.f:1826-1827`: "Package-local
  resolution waits on the TYPEFAMILY declaration grammar (PLAN item 6)".

Consequence for the census: the SUMV registry that item 8 consumes exists
(`src/core/type-family.f`), but there is **no declaration surface that populates
it with a real sum**, no hidden-field expansion (item 7) to make constructor
signatures sound, and no layout-aware stack ops (item 12) to lower a
multi-cell bundle. Item 8 as specified cannot land before 6, 7, and 12. This
census therefore maps (a) the registry slots item 8 must fill, (b) the checker
signature-registration machinery item 8 must drive, and (c) the exact
trust-emitting definer paths it must avoid — plus the missing prerequisites as
concrete gaps.

---

## 1. How constructor-like words are created today

There is **no** generated-constructor / defining-word combinator today. Four
existing mechanisms create "a word plus a stack effect"; item 8 must reuse the
checked one and avoid the three trusted ones.

### 1a. Plain checked `:` — the ONLY zero-trust publish path (the target)

The checked definer is `:` (there is no `CHECKED:` word; `rg CHECKED:` over
`src/ lib/ tools/` = 0 hits — it is prose only in `CLAUDE.md`). Publish dispatch:

- `habu2.f:3108 EM-COMPILE-PUBLISH` — `9 DATA HOOK-CELL LDR, 9 checked CBNZ,`
  routes a word with the checker HOOK installed to `EM-COMPILE-PUBLISH-HOOKED`.
- `habu2.f:3093 EM-COMPILE-PUBLISH-HOOKED` — pushes `BODYBUF-OFF` / `BODYLEN`
  and `9 BLR` calls the HOOK (checker `CHECK`); `10 G-POP 10 rejected CBZ,`
  reads the verdict and rolls back on reject. This is the checked body path:
  the `( in -- out )` declared sig is verified against the actual body.
- The declared sig is captured for this word at colon-open by
  `habu2.f:1660 C-COLON-MAYBE-SIG` → `C-SIG-CAPTURE-TSIG` (`habu2.f:1325`) into
  `TSIG-A-CELL / TSIG-U-CELL` (`layout.f:65-66`).

**Item 8's constructor must publish through this path** (a checked `:` body of
literal pushes), NOT through 1b/1c/1d. `docs/type-families.md:779-781`: "The
generated constructor body is checked code. The generator must not emit `TRUST`,
`TRUSTED:`, `set-check`, or require a `TRUSTED.md` manifest row".

### 1b. `TRUSTED:` — declared sig, body unchecked (FORBIDDEN for item 8)

- Keyword `trusted:` at `habu2.f:938` (`LKWTRUSTED ... s" trusted:"`), dispatch
  `habu2.f:3010` (`['] C-TRUSTED CF-ENTRY`).
- `habu2.f:3074 EM-COMPILE-PUBLISH-TRUSTED` — `10 DATA TRUSTED-CELL LDR, 10
  ttrusted CBNZ,` and `C-CALL-TRUST-PEND` (`habu2.f:1142`) stamp the declared
  `TSIG` via the checker's `TRUST` word without ever running the body checker.
- The `TRUSTED-CELL` flag (`layout.f:72`) is set by `C-TRUSTED`
  (`habu2.f:1691 12 1 MOVZ, 12 DATA TRUSTED-CELL STR,`).
- Checker side: `checker.f:4909 : TRUST {: na nu sa su :}` →
  `CHECKER-USIG-ADD` registers the effect from a source string (§3). This is the
  "native escape hatch (PLAN's TRUSTED:)" — `checker.f:4906-4907`.

### 1c. `CREATE` / `DOES>` — per-created-word runtime TRUST stamping (FORBIDDEN)

The DOES> factory stamps EACH created child's runtime signature through the
checker's `TRUST` word at runtime — a live trust site per instance:

- `habu2.f:1355 J-DOES` → `C-PARSE-CREATED-SIG` (`habu2.f:1335`) parses the
  child sig into `TCSIG-A/U-CELL` (`layout.f:67-68`), then `C-EMIT-CRSIG-SET`
  (`habu2.f:1256`) copies it into the created word's `CRSIG-A/U-CELL`
  (`layout.f:69-70`).
- `habu2.f:1400 EMIT-DOESPATCH` — at each `CREATE`, `9 DATA CRSIG-U-CELL LDR, 9
  nocr CBZ,` → `C-CALL-TRUST-LASTC` (`habu2.f:1149`) calls the checker `TRUST`
  hook (`C-FIND-TRUST`, `habu2.f:1106`, resolving `s" trust"` from
  `habu2.f:940`) to register the child's effect. `CRSIG` = the DOES>-child
  runtime signature; `TCSIG` = the creator's captured child sig.
- `habu2.f:1208 C-CALL-CHECK-DOES` / `habu2.f:1226 C-CALL-CHECK-DEFINER` verify
  the DOES>-body but the child effect itself is trust-declared, not inferred.
- `TRUSTED.md:8` names `CREATE`/`DOES>` explicitly as a trust category: "Trust
  only what cannot be inferred — host primitives, raw code emitters,
  `CREATE`/`DOES>`, and recursion".

The signature-carrying friend cells (task's "friend-arena cells"): `layout.f:65
TSIG-A-CELL` .. `layout.f:70 CRSIG-U-CELL`, plus `layout.f:71 DOESB-CELL`,
`layout.f:72 TRUSTED-CELL`, `layout.f:115 DOESP-CELL`, `layout.f:116
CREATEP-CELL`. All are zeroed in the snapshot builder
(`snap-lib.f:140-145`).

### 1d. `deftype`-style `evaluate` of a generated `TRUSTED:` shape (FORBIDDEN model)

The closest existing "generate a word from metadata" pattern — and the
anti-pattern item 8 must not copy:

- `roles.f:32 TRUSTED: DTC-EVAL ( -- ) DTC-BUF DTC-U @ evaluate ;` — one audited
  trust boundary reused for all deftypes.
- `roles.f:34 DEFTYPE-CAST-IN` / `roles.f:39 DEFTYPE-CAST-OUT` build
  `s" TRUSTED: >" ... " ( n -- " ... " ) ;"` text and `DTC-EVAL` it.
- `TRUSTED.md:281` pins `DTC-EVAL` as a manifest row.

Item 8 may generate `: NAME ( sig ) 0 ;`-shaped **checked** source and evaluate
it, but the generated text must contain no `TRUST/TRUSTED:/set-check`
(`PLAN.md:616-618`), and it must NOT add a `DTC-EVAL`-like TRUSTED: wrapper
(`PLAN.md:571-577`, "do not add new `TRUSTED.md` rows for ADT constructors").

### 1e. Interpret-mode defining words (`create`/`variable`/`constant`)

`habu2.f:1425 C-DEFHOOK` runs the checker record-hook after the defining word
emits, dispatched by `LKWCREATE 6 C-DEFHOOK` (`habu2.f:1610`), `LKWCONST 8`
(`habu2.f:1638`). Constructors are NOT these — they are variant-specific words,
not `create`/`constant` — but this is the interpret-mode hook family item 8's
`SUMTYPE`/generator runs alongside.

---

## 2. Sum/variant registries + the tag/layout data a constructor bakes

All in `src/core/type-family.f` (package TFAM, loaded unchecked after
type-schema.f — header `type-family.f:1-12`).

### 2a. TFAM (family) record — item 8 reads kind/arity/layout/variant-range

`type-family.f:148-167 BEGIN-STRUCTURE TF-REC`. Fields item 8 consumes:
- `TF.KIND` (`156`) — must be `TK-SUM` (`type-family.f:17`) or `TK-ENUM`
  (`type-family.f:18`); readers `TFAM-SUM?` (`216`), `TFAM-ENUM?` (`217`),
  `TFAM-LAYOUT?` (`218`).
- `TF.ARITY` (`154`) → `TFAM-ARITY@` (`197`) — parameter count for the
  `result<a,b>` sig the constructor declares.
- `TF.LAYOUT` (`157`, default `TL-STACK-CELL-TAG` `type-family.f:24`, set at
  decl `type-family.f:310`) → `TFAM-LAYOUT-POLICY@` (`199`). Item 8 handles
  only `stack-cell-tag` (spec §22.1 / `type-family.f:22-24`).
- `TF.SLOTS` (`159`) = max-payload-cells `M` → `TFAM-SLOTS@` (`200`); friend
  writer `TFAM-SLOTS!` (`225`). **Not populated by any current path** (0 at
  decl, `type-family.f:311`) — item 6/7 must set it; item 8 needs it for
  zero-padding width `M - p`.
- `TF.VAR-START` / `TF.VAR-COUNT` (`159-160`) → `TFAM-VAR-START@` (`201`),
  `TFAM-VAR-COUNT@` (`202`); friend writer `TFAM-VAR-RANGE!` (`226`). Index +
  count into SUMV — the constructor set to generate.
- `TF.TAGW` (`163`, default `TAGW-CELL` = 1 cell `type-family.f:38`, set
  `type-family.f:314`) → `TFAM-TAGW@` (`205`).

### 2b. SUMV (variant) record — item 8 both READS and must WRITE the ctor cells

`type-family.f:323-334 BEGIN-STRUCTURE SUMV-REC`:
```
CELL +FIELD SV.FAM          \ 324  owning family-id
CELL +FIELD SV.NAME-OFF     \ 325  interned variant tail ("ok"/"err")
CELL +FIELD SV.NAME-U       \ 326
CELL +FIELD SV.TAG          \ 327  tag value (0,1,2,...) baked as the top cell
CELL +FIELD SV.SCH-START    \ 328  payload schema range (type-schema nodes)
CELL +FIELD SV.SCH-COUNT    \ 329
CELL +FIELD SV.PAYCELLS     \ 330  p = payload cell count for THIS variant
CELL +FIELD SV.CTOR-SYM     \ 331  generated-constructor symbol   <-- item 8 fills
CELL +FIELD SV.CTOR-PKG-OFF \ 332  constructor package name off   <-- item 8 fills
CELL +FIELD SV.CTOR-PKG-U   \ 333  constructor package name len   <-- item 8 fills
```
Readers exist for FAM/NAME/TAG/SCH/PAYCELLS (`type-family.f:356-362`). **The
three `SV.CTOR-*` cells have NO readers and NO non-zero writer** — the only
reference is `SUMV-ADD` zero-initializing them: `type-family.f:386 0 r
SV.CTOR-SYM ! 0 r SV.CTOR-PKG-OFF ! 0 r SV.CTOR-PKG-U !`. These fields exist
precisely as the item-8 landing slots for `generated-constructor-symbol` /
`constructor-package-id` (spec §7 `type-family.f`→`docs/type-families.md:358-359`).
Item 8 must add friend writers/readers mirroring `TFAM-VAR-RANGE!` /
`TFAM-SLOTS!`.

`SUMV-ADD` (`type-family.f:375`) is the variant creator: `( fam name-a name-u tag
sch-start sch-count paycells -- id )`, rejects non-canonical tails
(`TF-REQUIRE-CANON` `377`) and duplicates (`E-TFAM-DUP` `378`). Query `SUMV-FIND`
(`368`), `SUMV-TAG@` (`359`), `SUMV-PAYCELLS@` (`362`).

### 2c. What a generated constructor bakes (spec §5 / §12)

Per `docs/type-families.md:262-263`: constructor output =
`payload-cell-0 ... payload-cell-(p-1) padding... tag`, tag top-of-stack
(`:250`). Concretely (`:706-712`): "payload cells are already on stack; push
`M-p` zero padding cells; push tag". So the generated body for variant `V` of
family `F` is: keep `p` inputs, push `(M - p)` literal `0`s, push literal
`SV.TAG@`. Examples pinned at `docs/type-families.md:721-777`
(`RESULT:OK` → `a 0`, `OPTION:NONE` → `0 0`, `OPTION:SOME` → `a 1`).

### 2d. Where ;VARIANT lands per the spec

`;VARIANT` terminates each variant block; `;SUMTYPE` terminates the sum
(`docs/type-families.md:499-516`, §9.2):
```
SUMTYPE result 2
  VARIANT ok  a ;VARIANT     \ :500
  VARIANT err b ;VARIANT     \ :501
;SUMTYPE                     \ :502
```
Enum is a zero-payload sum with the same terminated form
(`docs/type-families.md:530-538`). Item 6 (open) owns installing these tokens;
item 8 only consumes the SUMV rows they produce. PLAN reserves the tokens at
item 6: `PLAN.md:434-435` "item 6 reserves `TYPEFAMILY`, `SUMTYPE`, `VARIANT`,
`;VARIANT`, `;SUMTYPE`".

---

## 3. Checked public signature WITHOUT a trust row

Item 8 must give each constructor a public effect the checker synthesizes from
TFAM/SUMV metadata (not parsed from a trusted source string). The
signature-registration machinery lives in `src/core/checker.f`.

### 3a. Symbol interning + record-symbol binding

- `checker.f:2673 SYM-INTERN ( ptr u8 n n ptr u8 n -- n )` — intern a
  (package, vis, name) symbol; wrappers `SYM-GLOBAL` (`3218`, `3623`),
  `CHECKER-PUBLIC-SYM` (`3628`), `CHECKER-PRIVATE-SYM` (`3635`).
- `checker.f:3640 CHECKER-RECORD-SYM ( ptr u8 n -- n )` — resolve/intern the
  symbol for a name in the active package.
- `checker.f:3781 CHECKER-RECORD-NAME` — `CHECKER-RECORD-SYM CHECKER-REC-SYM !`
  sets the current record symbol (`CHECKER-REC-SYM` var `checker.f:2735`), which
  every effect append keys on (`checker.f:2892 CHECKER-REC-SYM @ r@ ER.SYM !`).

### 3b. Effect-record build/append (the sound path to reuse)

- `checker.f:2900 E-BUILD-EFFECT ( din dout rin rout hasr -- off )` — builds a
  USIGS effect record from **structured rows** (`din`/`dout` are type-graph
  offsets, not text). `checker.f:2148-2149`: "Textual signatures are
  source-boundary input only; checker-owned token semantics construct rows
  directly." Row builders: `STEP-TYPE-OUT/IN` (`2150/2156`), `MK-PUSH`,
  `MK-CON`, `MK-ROW`, `FRESH MK-VAR`.
- `checker.f:2920 E-ADD-EFFECT` — build + index by `CHECKER-REC-SYM` via
  `off 1 + CHECKER-REC-SYM @ HIDX-EFF!` (`2923`). This is the sound,
  metadata-driven registration (no source string, no trust).
- Text path (AVOID for the effect itself): `checker.f:2936 E-PARSE-ADD` →
  `checker.f:2943 USIG-ADD` → `checker.f:3788 CHECKER-USIG-ADD` (used by the
  `TRUST` word `checker.f:4911`). Item 8 must build the effect via
  `E-ADD-EFFECT`/structured rows OR by publishing a checked `:` body whose
  declared sig is verified — never via `CHECKER-USIG-ADD` on a fabricated
  string (that IS the trust mechanism).

### 3c. What the checker must synthesize per constructor

Declared effect `RESULT:OK ( a -- result<a,b> )` where the OUTPUT type
`result<a,b>` is a `T-PARAM` carrying the resolved `family-id`
(`checker.f:2861 x E-RES PARAM>FAM r@ E-PTR EN.H ! \ resolved family-id`). But
`result<a,b>` as an OUTPUT must **expand to hidden physical fields**
`@result.slot0<a,b> @result.tag<a,b>` (spec §10, `docs/type-families.md:567`)
so the checker equates the 2-cell body (`slot0` + `tag`) to the logical output.
That expansion is **item 7** (`PLAN.md:546-569`, `PUSH-LOGICAL` /
`LAYOUT-PUSH-FIELDS`) — not present (see §0). Reject-only hidden-field kind and
the rule that one-cell primitives cannot touch layout rows until item 12 land is
`PLAN.md:555-557` + `docs/type-families.md:604-611`.

### 3d. What item 12 must provide before user-callable constructors land

`PLAN.md:599-601`: "This item remains metadata-only until item 12 lands: no
public constructor package, private `construct` form, or runtime constructor
body is enabled before native and Gforth width-aware lowering can preserve
bundles." Item 12 makes every stack primitive layout-width-aware
(`PLAN.md:760-844`) and adds the checker-before-emission width path
(`census-tfam-12.md` §6: body is emitted per-token BEFORE the checker runs at
`;`, so the generated constructor's multi-cell push cannot be lowered soundly
until item 12's width facts reach the emitter). Until then, item 8 is
**metadata-only** — it fills SUMV `CTOR-*` and derives package names, but does
not publish a callable word.

Dependency note pinned: `PLAN.md:1033-1035` "Item 7 installs hidden-field
metadata in reject-only form. User-callable constructors, public layout rows,
and `MATCH` lowering wait until item 12 can preserve bundles".

---

## 4. Runtime tag validation (items 9/10) — what item 8 must leave behind

Tag checking / `MATCH` is items 9 (checker protocol) and 10 (native+Gforth
lowering + bad-tag runtime death). Item 8 is upstream and must leave the
artifacts they consume:

- **SUMV metadata keyed by (family-id, variant-id), not bare words.**
  `PLAN.md:595-598`: "The metadata is keyed by family id and variant id, not by
  bare variant words, so private same-tail variants cannot collide or leak."
  `SUMV-REC.SV.FAM`+tag+`SV.CTOR-SYM` (`type-family.f:324,327,331`) are that key.
- **Private constructor metadata only; `construct` token is item 9.**
  `PLAN.md:594-596`: "This item records private constructor metadata only; item
  9 introduces the source-level `construct family variant` token protocol."
  Spec §12 `docs/type-families.md:691-704`. `construct`/`MATCH`/`;MATCH`
  reserved at item 9 (`PLAN.md:436`); item 9 must first rename pre-existing
  `CONSTRUCT` in `lib/task.f` (`PLAN.md:648-651`).
- **Constructor package name derivation is pinned and shared byte-identically**
  across native `habu2` / `habu1` / Gforth mirror. Algorithm (§Package Shape
  `PLAN.md:89-102`, spec `docs/type-families.md:674-687`): uppercase each
  canonical package path segment + family tail, escape literal `-` as `--`, join
  with single `-`; if it exceeds the dictionary name limit, `T` + first 16
  lowercase hex of SHA-256 over the length-prefixed unescaped segment list +
  `-` + uppercase tail. Injective, stable, never allocation-order ids, never raw
  hyphen concat (so `A-B`+`c` ≠ `A`+`b-c`). Item 10 relies on this same spelling
  to name the lowered constructor/helper roots.
  - Reusable primitive present: `SHA256` / `SHA256-FILE-HEX` in
    `src/core/sha256.f` (used at `tools/hb-build-lib.f:718,722`,
    `tools/seed.f:79`). No package-derivation / hyphen-escape helper exists yet
    (`rg 'DERIVE-PKG|hyphen-escape|CTOR-PKG-NAME'` = 0 hits) — item 8 builds it.
- **Dictionary name-length limit** for the SHA-256 fallback trigger:
  `layout.f:14 DNAME-INL = 16`, `layout.f:15 DNAME-LEN-MASK`.
- **Reserved / non-reopenable generated package.** `PLAN.md:590-593` +
  `docs/type-families.md:688-691`: an existing ordinary package or qualified
  wordlist with the derived spelling makes the family declaration fail;
  `undefine` of a generated constructor word or package entry rejects
  (`PLAN.md:630-632`).

Item 10's bad-tag death path (`PLAN.md:714-732`) and item 9's `CF-MATCH` frames
(`PLAN.md:657-665`) are downstream; item 8 leaves them the SUMV tag values
(`SV.TAG@`), payload cell counts (`SV.PAYCELLS@`), max width (`TFAM-SLOTS@`), and
the derived package/symbol.

---

## 5. PLAN item 8 acceptance restated as a checklist (files/words per item)

From `PLAN.md:605-639`. Each line: criterion → owning file/word.

1. `RESULT:OK/ERR`, `OPTION:NONE/SOME`, + an arbitrary third non-result/option/
   color sum type-check and run → new `SUMTYPE` fixtures (item 6 grammar) +
   generator in `src/core/checker.f`/new `src/core/sumtype.f`; publish via
   checked `:` (§1a) driven from SUMV rows (§2b).
2. Wrong payloads reject → checker effect built in §3b must unify payload types;
   negative fixtures in `test/type-family-suite.f`.
3. Multi-cell payload variants prove `M>1` width, zero padding, padding drop,
   stack order for `ptr u8 n` → `TFAM-SLOTS@` (`type-family.f:200`) = M,
   `SV.PAYCELLS@` (`362`) = p, body pushes `M-p` zeros + tag (§2c).
4. Raw tag constructors not exposed → no public `n -> tag` word; spec §12
   `docs/type-families.md:783` "Do not expose public unchecked converters".
5. `RESULT` package publishes only `OK`/`ERR`, closed to extra tails →
   sealed/closed constructor package (Package Shape `PLAN.md:61-65`); enforced
   at wordlist layer (item 2b, still open — dots `habu-tfam-2b-*`).
6. Restore caller runtime+checker package state from global/unrelated/reopened
   contexts → `CHECKER-PACKAGE` (`checker.f:3555`) save/restore around generate;
   runtime `PKG-PUB-CELL`/`PKG-PRI-CELL` (`layout.f:123-124`, saved/restored in
   `C-FIND-GLOBAL` `habu2.f:1184-1189`).
7. Same-tail families in different packages publish disjoint APIs, cannot collide
   on `RESULT:OK` → package name derivation (§4) keyed on family-id
   (`TFAM-FIND-IN` `type-family.f:246`, `SV.FAM`).
8. **trust-lint, trusted-inventory, checked-boundary-lint, evaluated-source
   capture, generated-constructor audit prove NO generated trust sites** →
   `tools/trust-lint-core.f`, `tools/trusted-inventory.f`,
   `tools/checked-boundary-lint-core.f`; must scan generated/evaluated source
   (`PLAN.md:616-620`). No `TRUSTED.md` rows added (`PLAN.md:577`, `:621`).
9. Injective/stable package-name fixtures (`A-B`+`c` vs `A`+`b-c`; unrelated
   earlier decl does not rename) → derivation helper (§4) + fixtures.
10. Private `SUMTYPE result` in `package PKG` exports no external package; two
    private same-tail families share a variant tail without bare `OK` →
    visibility check `TFAM-VIS@` (`type-family.f:196`) `CHECKER-PACKAGE-PRIVATE`;
    metadata-only (§4).
11. `undefine RESULT:OK` / generated words / package entries reject →
    `CHECKER-UNDEFINE` (`checker.f:4026`) must refuse protected constructor
    symbols.
12. Linear-payload constructors reject until item 11 → `PLAN.md:602-604,638-639`;
    hook to `LAYOUT-LINEAR?` (item 11, open).
13. No manifest rows added; native execution proven here; Gforth parity with
    item 10 → gate `17i` (§6 below).

Paths declared for item 8: `PLAN.md:572-573` `src/core/checker.f`,
`src/habu/habu2.f`, `docs/type-families.md`, `tools/trust-lint-core.f`,
`TRUSTED.md`. Per Package Shape `PLAN.md:124-131`, new registries/generators
should land in package-owned files (e.g. `src/core/sumtype.f`) with
`tools/srclist.f`, `FILEMAP.md`, build-cache keys updated — `type-family.f` is
already registered (`FILEMAP.md:44`) as the template.

### Gate 17i checklist (item 17 applied to item 8, `PLAN.md:958-1021`)

- TDD red fixtures before implementation; rebuild `bin/hb`; native
  self-refresh/fixpoint byte-identical (`PLAN.md:968-970,995-997`).
- **Trust ratchet: exact trust manifest + inventory counts before/after item 8
  must be unchanged** (`PLAN.md:989-992`): "The type-family/ADT campaign may not
  add `TRUST`, `TRUSTED:`, `set-check`, or `TRUSTED.md` rows". `TRUSTED.md`
  currently has ~520 rows (`rg -c '^\| ' TRUSTED.md`) — item 8 leaves that count.
- `checked-boundary-lint` must treat `TRUSTED: ... set-check` as generated
  checker mutation; `trust-lint` must scan after backslash bytes in string/path
  literals and share the trusted-inventory full-file lexer (`PLAN.md:982-989`).
- Evaluated/generated/object-emitted/materialized-loader source scanned, not
  only raw files; fixtures `S\" ... TRUST ...\" evaluate` fail the ratchet
  (`PLAN.md:1009-1012`).
- filemap/host/public-signature lints; candidate binary size vs
  `test/gate-build-size.f` (`PLAN.md:993,997-999`); no-binary Gforth bootstrap
  reaches fixpoint (`PLAN.md:995-996`).

---

## 6. Trust surface around defining words + zero-new-trust confirmation

Trust today is a source-scanned manifest, not a runtime table:

- `TRUSTED.md:1-9` — `TRUST` declares an effect without checking the body;
  `TRUSTED.md:8` lists `CREATE`/`DOES>` and raw emitters as trusted categories.
- `TRUSTED.md:27-29` — `tools/trust-lint.f` enforces the manifest for every
  `TRUST` site in `src/` and `lib/`; `--source-list` runs `trust-lint
  source-only` per input.
- Defining-word trust sites that item 8 must NOT touch/extend: `TRUSTED:` publish
  (`habu2.f:3074`), DOES>-child runtime `TRUST` (`habu2.f:1149,1417`),
  `deftype`'s `DTC-EVAL` (`roles.f:32`, `TRUSTED.md:281`),
  `em-compile-publish*` emitter rows (`habu2.f:3091,3106,3117` — pre-existing
  image-builder trust, may be reused but not added to per `PLAN.md:1016-1020`).

**Confirmation:** Item 8 CAN land with zero new trust rows *provided* it (a)
publishes constructors through the checked `:`/`E-ADD-EFFECT` path (§1a/§3b),
and (b) if it uses `evaluate` to materialize `: NAME ...;` source, the generated
text contains no `TRUST/TRUSTED:/set-check` and reuses no new TRUSTED: wrapper.
The metadata-only-until-12 rule (`PLAN.md:599-601`) means the first landing does
not even publish a runtime body, so it trivially adds no trust. **Capability
tension to watch (see §7):** `evaluate` is itself checker-unsafe
(`checker.f:4914 a u s" evaluate" CORE-STR=` → `REJECT-UNSAFE`), so any generator
that shells out through `evaluate` needs an *existing* audited boundary; adding a
new `DTC-EVAL`-style TRUSTED: wrapper would violate the ratchet. The sound
alternative is a direct structured `E-ADD-EFFECT` registration + native emit,
avoiding `evaluate` entirely.

---

## 7. Open risks / unknowns — each with a probe

1. **Prerequisites 6/7/12 not landed (§0).** Item 8 references SUMV rows,
   hidden-field expansion, and width-aware lowering that do not exist.
   *Probe:* `rg '^: SUMTYPE|PUSH-LOGICAL|LAYOUT-PUSH-FIELDS' src/` (currently 0);
   run the item-6/7/12 dots first, or scope item 8 to metadata-only + fixtures
   gated behind them.
2. **`SV.CTOR-*` writers/readers absent.** Only zero-init exists
   (`type-family.f:386`). *Probe:* `rg 'SV.CTOR' src/` — confirm no reader; item
   8 must add `SUMV-CTOR!`/`SUMV-CTOR-SYM@`/`SUMV-CTOR-PKG$` mirroring
   `TFAM-VAR-RANGE!` and persist through `TFAM-SNAPSHOT-PERSIST`
   (`type-family.f:594-602`, which already bakes the whole SUMV arena — extra
   cells persist for free since they are integers/offsets).
3. **`TFAM-SLOTS` (M) never populated.** Constructor padding width `M-p` is
   unknown. *Probe:* `rg 'TFAM-SLOTS!' src/` (only the friend writer, no caller);
   item 6/7 must compute M = max variant `SV.PAYCELLS` and call `TFAM-SLOTS!`.
4. **Generation mechanism: `evaluate` vs direct emit.** Whether constructors are
   materialized as `: ...;` source (needs an audited non-trust evaluate boundary)
   or emitted structurally (native codegen + `E-ADD-EFFECT`). *Probe:* compare
   `roles.f:32 DTC-EVAL` (trusted evaluate) against `checker.f:2920 E-ADD-EFFECT`
   (structured, trust-free); the ratchet (`PLAN.md:989-992`) forces the latter or
   reuse of an existing boundary.
5. **Package-name derivation determinism across three engines.** Native `habu2`,
   `habu1`, and Gforth mirror must produce byte-identical spellings
   (`PLAN.md:584`, `docs/type-families.md:675-676`). *Probe:* a cross-engine
   fixture that derives the same package for `PKG:result` on all three; verify
   `src/core/sha256.f` is in every engine's source closure (`tools/srclist.f`,
   Gforth `bootstrap/`).
6. **Reserved/non-reopenable collision check.** The derived package must
   collision-check against existing packages and qualified wordlists
   (`docs/type-families.md:688-691`). *Probe:* `rg 'C-PACKAGE-ALLOC-WIDS|wordlist'
   src/habu/habu2.f` (`PLAN.md:175`) — determine where a name→wordlist lookup can
   reject before the family declares.
7. **Sealing (item 2b) still open — closed-but-callable package guarantee.**
   Acceptance criterion 5/8 depends on `TFAM`/generated packages being sealed at
   the wordlist layer, which is dots `habu-tfam-2b-*` (open). *Probe:* `dot list |
   rg 2b`; item 8's "package closed to extra tails" fixture cannot pass until 2b
   lands — flag as a cross-dependency.
8. **`undefine` protection of constructor symbols.** `CHECKER-UNDEFINE`
   (`checker.f:4026`) currently deletes any user sig; must reject protected
   generated symbols. *Probe:* write `undefine RESULT:OK` fixture; confirm it
   currently would succeed (soundness gap) and add the guard.

---

## Contradictions (PLAN/spec vs code)

- **C1 — Dependency order says 6/7/12 precede 8; tree is at item 4.** PLAN
  `1031` orders `... 7 -> 17g -> 12 -> 17h -> 8`, but `SUMTYPE/VARIANT` grammar
  (item 6), hidden-field expansion (item 7), and layout-aware ops (item 12) are
  unimplemented (§0). The census is written against a tree where item 8's inputs
  do not yet exist. Not a plan defect — a scheduling reality the implementer must
  honor (do not start item 8's runtime body before 12).
- **C2 — Spec §6 names `TFAM-ADD-INTERNAL` / `TFAM-FIND-INTERNAL`
  (`docs/type-families.md:320-321`); code names them `TFAM-DECL`
  (`type-family.f:293`) / `TFAM-FIND-IN` (`type-family.f:246`).** Same behavior,
  different spelling; spec is illustrative, code is authoritative.
- **C3 — Spec §7 lists the SUMV record ending at `constructor-package-id`
  (`docs/type-families.md:359`); code adds an extra split field
  `SV.CTOR-PKG-OFF`+`SV.CTOR-PKG-U` (`type-family.f:332-333`)** because names are
  interned offset+len, not a single id. Consistent with the interning design
  (`type-family.f:4-6`), but the "package-id" in spec is realized as an interned
  string pair, not a numeric wordlist id — item 8 must decide whether the derived
  package is stored as interned name (matches CTOR-PKG cells) or as a WID.
- **C4 — Item 8 `Paths` list omits `src/core/type-family.f`
  (`PLAN.md:572-573`)** yet the SUMV `CTOR-*` fields item 8 must populate live
  there, and Package Shape (`PLAN.md:124-131`) prefers new generators in
  package-owned files. Expect the implementer to edit/extend `type-family.f`
  (and likely a new `src/core/sumtype.f`) even though only `checker.f`/`habu2.f`
  are named.
- **C5 — Item 8 promises native execution "proven here"
  (`PLAN.md:589-590`) while also being "metadata-only until item 12 lands"
  (`PLAN.md:599-601`).** These coexist only if "native execution proven here"
  means the item-8 *commit that lands after 12* proves it; a strict reading of
  the two sentences is internally tense. Implementer should treat runtime body +
  native execution as gated on item 12 within the same dot.
