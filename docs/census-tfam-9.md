# TFAM-9 MATCH-Protocol Census — dot `habu-tfam-9-construct-2dd4f2d3`

Scope: PLAN.md item 9 "Add checker-owned `MATCH` token protocol and control
semantics" (`PLAN.md:646-688`) and its per-item proof gate `17j` (item 17,
`PLAN.md:958-1027`; gate slot `... 8 -> 17i -> 9 -> 17j -> 10 ...` at
`PLAN.md:1031`). Normative spec: `docs/type-families.md` §2 (`84-142`), §12
(`645-704`, constructors + `construct`), §13 (`787-811`, `MATCH` syntax), §14
(`815-904`, checker semantics), §15 (`908-966`, runtime tag), §16 (`970-1037`,
lowering). Every claim is `file:line` + a quoted snippet. **Symbol names are
authoritative; line numbers drift — anchor to the symbol.** Repo root
`/Users/joel/Work/habu`; paths relative here for density, absolute in the summary.

Spelling note (see Contradictions C2): the spec/PLAN token is **`;MATCH`**
(`docs/type-families.md:802,885`; `PLAN.md:436`), not `END-MATCH`.

---

## 0. STATE / PREREQUISITE ALERT (read first)

Item 9 declares `Depends on: items 7, 8, and 12` (`PLAN.md:687`) and the chain
routes `... 7 -> 17g -> 12 -> 17h -> 8 -> 17i -> 9` (`PLAN.md:1031`). **None of
6/7/8/12 are landed** — the tree is at item 4 (census-tfam-8 §0: latest TFAM
commit is `TFAM 4: registry-driven params + family-id terms`; open dots
`habu-tfam-5/6/7/8/12`). Direct evidence:

- No `SUMTYPE`/`TYPEFAMILY`/`VARIANT`/`;SUMTYPE` defining word exists
  (`rg '^: SUMTYPE|^: VARIANT|^: TYPEFAMILY'` = 0) — item 6 grammar absent.
- `MATCH`/`;MATCH`/`construct` are **undefined and definable** today (probes
  below): the checker rejects *usage* fail-closed, but nothing *reserves* the
  spellings and no `CF-MATCH` frame or capture exists.
- No `src/core/match.f`/`src/core/sumtype.f` (`ls` = "No such file"); Package
  Shape wants `src/core/match.f` (`PLAN.md:128`).

**Fail-closed proof for the exact command path** (`printf … | bin/hb --load
/dev/stdin`, `bin/hb` = 115831 bytes, mtime Jul 4):

```
: RC ( n -- n ) MATCH result ok OF drop 0 ENDOF ;MATCH ;   -> exit 70, "E-UNDEFINED: MATCH"
: MK ( a -- n ) construct result ok ;                        -> exit 70, "E-UNDEFINED: construct"
: U  ( result<n,n> -- n ) drop 0 ;                           -> exit 70, "unknown type 'result' in signature"
: construct ( -- ) ;                                         -> exit 0   (definable — NOT reserved)
: MATCH ( -- ) ;                                             -> exit 0   (definable — NOT reserved)
```

So `MATCH`/`construct` *used* in a checked body reject as undefined words
(`DO-TOK` → `E-UNDEFINED`, the miss class is closed); the open soundness gap is
that the spellings are **not reserved** (item 9 must add them) and there is no
control semantics. This census maps (a) the token-dispatch seam item 9 hooks,
(b) the SUMV metadata exhaustiveness reads, (c) the checker-only vs
native-lowering (item 10) boundary, and (d) the friend-only buildable slice.

---

## 1. Token-protocol machinery MATCH hooks into

### 1a. The per-token checker dispatch (`DO-TOK1`) — the exact insertion seam

`checker.f:4976 : DO-TOK1 {: a u :}` is the one-token dispatcher inside
`CHECK-SCAN`. Its cascade (order is load-bearing):

```
4979  TOK0 @ IF … 0 TOK0 ! ELSE            \ first token = the word NAME (skipped)
4981  LMODE @ IF … LOC-TOK ELSE            \ locals-declaration mode
4982  … s" {:" … LOC-BEGIN ELSE
4983  … UNSAFE-TOK? IF REJECT-UNSAFE ELSE
4984  … s" is" … IS-TOK ELSE               \ `is` consumes the FOLLOWING token
4988  … LOC-REF? 0= IF                     \ local variable reference
4989  … CF-TOK? 0= IF                      \ control-flow keyword recognizer
4990  … RS-TOK? 0= IF                      \ return-stack words
4991  … DO-TOK                             \ NORMAL dictionary lookup / undefined
```

Item 9's "constructor/match-mode token capture **before normal dictionary
lookup** so branch names cannot collide with locals or words" (`PLAN.md:660-661`)
must be inserted **before `LOC-REF?` (`checker.f:4988`)** — locals are resolved at
4988, and the spec says variants "are not ordinary word lookups"
(`PLAN.md:655`). Capturing at `CF-TOK?` (4989) alone is too late: a variant
token spelled like a local would already have bound at 4988.

### 1b. `CF-TOK?` — the control-keyword recognizer where MATCH/;MATCH/OF land

`checker.f:4682 : CF-TOK? ( ptr u8 n -- bool )` string-matches every control word
and calls its checker model:

```
4685  a u s" if"    CORE-STR= IF CF-IF    RES-TRUE EXIT THEN
4688  a u s" case"  CORE-STR= IF CF-CASE  RES-TRUE EXIT THEN
4689  a u s" of"    CORE-STR= IF CF-OF     RES-TRUE EXIT THEN
4690  a u s" endof" CORE-STR= IF CF-ENDOF  RES-TRUE EXIT THEN
4691  a u s" endcase" CORE-STR= IF CF-ENDCASE RES-TRUE EXIT THEN
```

Item 9 adds `s" match"`/`s" endmatch"` arms here (or a sibling `MATCH-TOK?` run
before `CF-TOK?` in `DO-TOK1`), and **shares `of`/`endof`** — the spec reuses the
`OF … ENDOF` branch surface (`docs/type-families.md:796-801`). PLAN Risk:
"`CASE` and `MATCH` share `OF`/`ENDOF` surface; parser dispatch must distinguish
them without weakening either" (`PLAN.md:684-685`). `CF-OF`/`CF-ENDOF`
(`checker.f:4506,4515`) today assume a `case` frame (kind 7/8, §1e); they must
branch on whether the enclosing frame is `CF-MATCH`.

### 1c. `is` / `IS-TOK` — the working template for "consume the next token"

`MATCH family`, `construct family variant`, and `variant OF` each need the parser
to pull the FOLLOWING raw token(s) inline. The existing precedent is `is`:

- `checker.f:4968 : IS-TOK` calls `checker.f:4939 : IS-NEXT-TOKEN ( -- ptr u8 n
  bool )` which advances `TI`/`TBLEN` and returns the next whitespace-delimited
  token from the body text (`IS-SKIP-WS` `4933`).
- `MATCH` resolves `family` via `IS-NEXT-TOKEN` + `TFAM-RESOLVE*`
  (`checker.f:444-447`, hook installed by `type-family.f:652`). `construct` reads
  two tokens and resolves `(pkg,fam,variant)` (`SUMV-FIND`, §2). Each arm reads a
  `variant` token before matching `of`.

### 1d. Control-flow checker models (`CF-*`) — CASE is the CF-MATCH template

The CF stack is a fixed 32-row array `create CFS 32 CFS-REC * allot`
(`checker.f:4349`); `#CFC` is the depth; `CF-PUSH` (`4393`), `CF-TOP` (`4362`),
`CF-DROP` (`4416`), reset `0 #CFC !` at `CHECK-RESET` (`checker.f:5040`). Frame
record `CFS-REC` has 14 cells `CF.KND … CF.TXS` (`checker.f:4332-4347`). Kinds in
use (**literals, not named constants** — `checker.f:4329`):

```
1 if   2 if+else   3 begin   4 begin+while   5 do   6 quotation   7 case   8 of
```

Models: `CF-IF` (`4488`), `CF-CASE` (`4490`, pushes kind 7, `CF.DED`=per-index
"seen" flag), `CF-OF` (`4506`, `STEP-N-IN`; pushes kind 8), `CF-ENDOF`
(`4515`, `CF-CASE-ACCUM` folds the arm output), `CF-ENDCASE` (`4525`), plus the
join accumulator `CF-CASE-ACCUM` (`4494`) which unifies each live arm's DCUR/RCUR
via `SUNI`/`RSUNI`. **`CF-MATCH` is a new multi-arm kind (9+)** modeled on
kind-7 CASE: `CF-CASE` opens the frame, each `variant OF` opens an arm and
refines the payload, `ENDOF` accumulates, `;MATCH` unifies live arms and checks
coverage. The CASE machinery (`CF-CASE-HAS?`/`CF-CASE-HAS!`/`CF-CASE-DATA!`,
`checker.f:4375-4391`) is the direct template for the seen-set + per-arm output
rows the spec's match frame lists (`docs/type-families.md:835-847`).

**Overflow contradiction (C4):** `CF-PUSH` (`checker.f:4394`)
`#CFC @ 31 > IF -1 UNCK !` marks the definition *uncheckable* on overflow. PLAN
requires the opposite for `MATCH`: "Overflow must reject with diagnostics, not
silently mark a definition uncheckable" (`PLAN.md:662-663`). Item 9's `CF-MATCH`
allocation must reject, not follow `CF-PUSH`'s `UNCK` path.

### 1e. Where the checker is invoked (compile HOOK) — item 9 lives entirely inside it

The checker is a body-verification pass driven by a HOOK cell, not a codegen
step. `habu2.f:3108 EM-COMPILE-PUBLISH` routes a hooked word to
`habu2.f:3093 EM-COMPILE-PUBLISH-HOOKED`, which pushes `BODYBUF-OFF`
(`3096`), `BLR`s the HOOK (checker `CHECK`, `checker.f:5091`), and reads the
verdict `10 G-POP 10 rejected CBZ,` (`3099`), rolling back on reject. `CHECK`
(`5091`) → `CHECK-RESET` (`5037`) → `CHECK-SCAN` (`5050`, the `DO-TOK1` loop) →
`CHECK-VERDICT` (`5088`). **Item 9 adds no new compile step** — the whole MATCH
protocol is checker-internal token handling that runs when the HOOK fires. The
compiler-side keyword table (`CF-ENTRY` `habu2.f:2266`; `J-CASE`/`J-OF`/`J-ENDOF`
`habu2.f:3140-3143`; `EMIT-KWDATA` `habu2.f:910`) is **item 10's** surface, not
item 9's (`PLAN.md:690-713`).

---

## 2. Exhaustiveness checking — what the checker reads to prove arm coverage

All variant/family metadata lives in `src/core/type-family.f` (package TFAM,
loaded after checker.f via forward xt hooks — `type-family.f:650-653`).

### 2a. The variant set of a sum family

- Kind guard (spec §14 step 2, `docs/type-families.md:827`):
  `TFAM-SUM? ( id -- bool )` (`type-family.f:216`), `TFAM-ENUM?` (`217`).
- **Variant index range**: `TFAM-VAR-START@ ( id -- n )` (`type-family.f:201`)
  and `TFAM-VAR-COUNT@ ( id -- n )` (`202`) give the contiguous SUMV id range —
  the enumeration item 9 walks for coverage. **But `SUMV-ADD` does NOT populate
  this range** (`type-family.f:375-387` sets only the variant row; the family's
  `TF.VAR-START/COUNT` stay 0). The friend writer `TFAM-VAR-RANGE! ( id start
  count -- )` (`type-family.f:226`) sets it, and only item 6/8 call it.
  *Probe evidence:* after `SUMV-ADD` × 2, `TFAM-VAR-COUNT@` still returned `0`.
  So item 9's range-based exhaustiveness depends on `TFAM-VAR-RANGE!` being
  called first, **or** it must scan all SUMV filtering by `SUMV-FAM@` (§2b).
- Family arity for arg recovery: `TFAM-ARITY@` (`type-family.f:197`).

### 2b. Per-variant reads (coverage + refinement)

`type-family.f:323-334 BEGIN-STRUCTURE SUMV-REC`; readers:
`SUMV-FAM@` (`356`), `SUMV-NAME$ ( id -- ptr u8 n )` (`357`), `SUMV-TAG@`
(`359`), `SUMV-SCH-START@`/`SUMV-SCH-COUNT@` (`360-361`, the payload schema
range for refinement), `SUMV-PAYCELLS@` (`362`), and the resolver
`SUMV-FIND ( fam name-a name-u -- id true | false )` (`368`) — how `variant OF`
resolves the branch token (spec §14 step 2, `docs/type-families.md:859`),
plus duplicate rejection (`SUMV-MATCH?` `365`; spec step 3, `:860`).

### 2c. The seen-variant bitset + per-arm output rows

Spec match-frame fields (`docs/type-families.md:835-847`): `family-id`, family
args, base/accumulated data+return rows, **seen variant bitset**, payload slot
count, live/dead state. `TF.VAR-COUNT` bounds the bitset. The CASE frame already
does per-index "seen" bookkeeping with `CF.DED` as the flag and `CF.SB`/`CF.RB`
as the accumulated arm rows (`CF-CASE-HAS?`/`HAS!`/`DATA!`/`RET!`,
`checker.f:4375-4391`) — item 9 reuses that shape or adds a growable/fail-closed
`CF-MATCH` arena (`PLAN.md:657-663`).

### 2d. Payload refinement at `OF` (schema instantiation)

Spec §14 `ok OF` step 5-6 (`docs/type-families.md:862-863`): "Instantiate variant
payload schema using family arguments. Set DCUR = base row + instantiated
payload." The schema nodes are `src/core/type-schema.f`: `SCHEMA-APP`
(`type-schema.f:95`, family application), `SCHEMA-PARAM` (`90`, param-ref),
`SCHEMA-CON` (`93`); readers `SCHEMA-TAG@`/`A@`/`B@`/`C@` (`100-103`). The checker
instantiation engine that substitutes params into effect nodes is `E-INST`
(`checker.f:3017`) / `E-INST-RESET` (`2981`) — the template for "instantiate the
SUMV payload schema range with the recovered family args and push it onto DCUR".
No `SUMV-payload-schema → row` helper exists yet — item 9 builds it (mirroring
`VREC-PUSH-FIELDS` `checker.f:1667`). Note `SC-QUOT` quotation-payload schemas
remain an open item-4 remainder (census-8 §0) — a refinement edge to gate.

### 2e. Where the reject diagnostics / repair classes belong

- Today's diag surface: `render.f:258 : DCODE` maps checker state to
  `E-MISMATCH`/`E-REJECTED`/`E-BAD-SIGNATURE`/… ; `render.f:279 : REPAIR-CLASS`
  maps to `fix_type`/`fix_return_stack`/`fix_signature_*` (`render.f:283-298`).
  There is **no MATCH-specific code or class** — a non-exhaustive or ill-typed
  arm reject surfaces through the generic `E-REJECTED`/`E-MISMATCH` path (exactly
  as item 7's layout rejects do, census-7 §3c).
- Dedicated ADT classes (non-exhaustive-match, duplicate-variant, wrong-family,
  branch-output-mismatch, missing-variant/family token) are **item 13's** job —
  `PLAN.md:846-891` "Add … machine-readable ADT fields/classes … variant/tag …",
  and item 13 `Depends on: items 5, 7, 8, 9, and 12` (`PLAN.md:890`). The class
  registry is `docs/repair-diagnostics.md:86-114` (current classes) +
  `:120-134` (`GJA-SUGGEST-FOR` suggestion table). Item 9 should **dot** the new
  ADT repair classes for item 13 and surface rejects through the existing
  machinery meanwhile (do not invent a one-off class in item 9).

---

## 3. The construct-side and the item-9 / item-10 boundary

### 3a. What "construct" means (spec)

`construct family variant` is the **private** introduction form (public families
also publish `RESULT:OK`-style package words — item 8). Spec §12
(`docs/type-families.md:691-704`): "The parser consumes the family and variant
tokens, resolves them while the owning package is open, and records
`(owning-package-id, family-id, variant-id)` for checker effects and
native/Gforth lowering." Runtime rule (`:706-712`, §16 `:974-988`): payload cells
already present → push `M-p` zero padding → push tag `k`, where `M = TFAM-SLOTS@`
(`type-family.f:200`), `p = SUMV-PAYCELLS@` (`362`), `k = SUMV-TAG@` (`359`).

Metadata keying: private construct is keyed `(pkg,fam,variant)`, **not** by the
`SV.CTOR-SYM`/`SV.CTOR-PKG-OFF`/`SV.CTOR-PKG-U` cells (`type-family.f:331-333`) —
those are item 8's *public* constructor package cells (census-8 §2b). PLAN:
"item 8 records private constructor metadata only; item 9 introduces the
source-level `construct family variant` token protocol" (`PLAN.md:594-596`).

### 3b. Item 9 produces these artifacts for item 10 to lower

Item 9 is **checker-only** (spec §14 is all checker steps; codegen is §16).
Concretely, item 9 hands item 10:

1. **Reserved tokens** `construct`, `MATCH`, `;MATCH` (+ the shared `of`/`endof`
   branch surface) — `PLAN.md:436` reserves them at item 9;
   `tools/reserved-name-lint-core.f:107-129 RNL-RESERVED-CONTROL?` is where they
   register (currently absent — §6 R1).
2. **The resolved `(owning-package-id, family-id, variant-id)` capture** per
   `construct` (`PLAN.md:656-658`; spec `:699-701`) — the checker/compiler
   capture item 10 lowers to `push M-p zeros + tag`.
3. **`CF-MATCH` frame metadata**: family-id, family args, seen bitset, per-arm
   output rows, dead-path state, tags (`SUMV-TAG@`), payload widths
   (`SUMV-PAYCELLS@`), family max width (`TFAM-SLOTS@`) — the numbers item 10's
   compare/branch + bad-tag die path consume (`PLAN.md:690-732`; spec §15-16).
4. **The checked effects** (constructor `( p… -- family<args> )`; `MATCH`
   `( family<args> -- joined-output )`) so item 10's width-aware emit
   (item 12) knows the stack shape.

### 3c. What item 10 owns (NOT item 9)

Native + Gforth lowering: `EMIT-KWDATA` rows, label vars, `J-MATCH`/`J-SEMIMATCH`,
tag compare/branch chains, invalid-tag die-with-no-continuation, and the
**compiler-side** match-mode token capture "before the normal
local/keyword/literal/call/undefined path in both compilers"
(`PLAN.md:690-713`, esp. `:706`). No `MATCH`/`construct`/`endmatch` string
appears in `src/habu/habu2.f` or `bootstrap/cg/forth.fs` today (only unrelated
"match" substrings). Item 10's CASE-shape parity target is
`tools/compiler-dispatch-test.f:30-37,129-136` (`J-CASE`/`J-ENDCASE` MUST-HAVE) —
item 10 adds the `J-MATCH` analogues there.

---

## 4. Dependencies consumed + friend-only buildable slice

- **Consumes item 6** (`SUMTYPE`/`TYPEFAMILY` grammar, unbuilt): real
  `SUMTYPE result … ;SUMTYPE` fixtures and the `TFAM-VAR-RANGE!` call that
  populates the variant range item 9 enumerates (§2a).
- **Consumes item 7** (hidden physical fields, unbuilt): spec §14 steps 3-6
  (`docs/type-families.md:828-831`) — `MATCH` "verifies physical top cells match
  the hidden layout … recovers family arguments from hidden field types … pops
  hidden physical fields from DCUR". Without item 7's layout expansion there is no
  hidden-field row to consume/refine.
- **Consumes item 8** (constructor metadata, unbuilt): SUMV `CTOR-*` cells and
  the family-id/variant-id keying `construct` resolves against.
- **Consumes item 12** (width-aware lowering, unbuilt): "Public `MATCH` checking
  for layout values is not enabled until item 12 proves width facts reach native
  and Gforth lowering; before that, `MATCH` parser/capture metadata may exist
  only as reject-only scaffolding" (`PLAN.md:663-665`). Linear-payload `MATCH` is
  gated on item 11 (`PLAN.md:666-668`).

**Friend-only buildable (as census-7/8 found):** `TFAM-DECL`, `SUMV-ADD`,
`TFAM-SLOTS!`, `TFAM-VAR-RANGE!` are reachable from user source today (item 2b
sealing is open) — *probe:* a top-level `s" " CHECKER-PACKAGE-PUBLIC s" myr" 2
TK-SUM TFAM-DECL … 1 TFAM-SLOTS! … s" ok" 0 -1 0 0 SUMV-ADD` ran to exit 0 and
`TFAM-SUM?`/`SUMV-FIND` read back correctly. So item 9's **checker protocol,
`CF-MATCH` frame, exhaustiveness, and reject scaffolding are testable ahead of
item 6** via synthetic registration (matching `test/type-family-suite.f:49-58`'s
`TFAM-DECL` pattern). But the *full* MATCH-over-layout path stays reject-only
until item 12; the buildable item-9 slice is: reserve tokens + `construct`
resolution + `CF-MATCH`/exhaustiveness on synthetic sums + reject-only gating.

---

## 5. Trust surface + rollback interaction

### 5a. Rollback — MATCH arms are per-check, NOT registry rows

The transactional registry rollback stack is `RBF-REC` (`checker.f:5145-5162`),
`RBF-PUSH` (`5195`), `RBF-POP` (`5218`), driven by `CHECKER-SCOPE-START/DONE`
(`5242/5245`) and `CHECK-CANDIDATE-START/DONE` (`5248/5253`); the TFAM/SUMV/SCHEMA
registries hang parallel marks off `REG-EXT-RB-SAVE-XT`/`-RESTORE-XT`
(`checker.f:5142-5143`, installed by `type-family.f`). **`MATCH` arms open/close
on the `CFS` control-frame stack** (`checker.f:4349`, reset per check at
`CHECK-RESET` `5040`), which is *per-definition* transient state, **not**
registry rows. `construct` records a `(pkg,fam,variant)` tuple (capture), not a
registry insert. Therefore **item 9 needs NO new `RBF-*` field** — it adds no
persistent registry counter. (Contrast item 3, which added `TFAM/SUMV/SCHEMA`
high-water marks.) The only rollback interaction: a `MATCH` def rejected inside
an all-errors candidate must leave the CF stack clean, which `CHECK-RESET`
(`5040 0 #CFC !`) already guarantees on the next check, and `CHECK-CANDIDATE!`
(`5263`) already pops its `RBF` frame under `catch` (`5266-5268`).

### 5b. Trial / unification machinery the arm join composes with

Branch-output unification at `;MATCH` (spec §14 step 4,
`docs/type-families.md:894`) uses the same primitives as CASE's join:
`SUNI` (`checker.f:4424`) / `RSUNI` (`4439`) wrap `UNIFY`, exactly as
`CF-CASE-ACCUM` (`4494`) folds live arms. The speculative-binding trail
(`TV!` `checker.f:220`, `TRY-EFF` `4131`, `TRIAL-SAVE` `4096`, `TRIAL-REST`
`4119`, `TRIAL-DEPTH` `215`) is for prim-overload trials — the MATCH join reuses
`UNIFY` directly and needs **no new trial machinery**. Payload refinement builds
`PARAM`/`T-CON` terms in the per-check arena (grown by `PARAM-ENSURE`), which is
reset per check — no rollback counter involved.

### 5c. Zero-new-trust confirmation

Item 9 Paths = `src/core/checker.f`, `lib/task.f`, `docs/type-families.md`
(`PLAN.md:647`). Trust rows in scope:

- `checker.f` — pre-existing `TRUSTED:` raw-cell→pointer arena converters
  (unrelated to ADTs, census-7 §5); item 9 adds pure checker logic (token
  dispatch, CF-MATCH frame, unify), **no new trust**.
- New `src/core/match.f` (Package Shape, `PLAN.md:128`) — a checker-prefix core
  file, checked-checker logic, zero trust (like `type-family.f`, which has zero
  trust rows).
- `lib/task.f` — has `TRUSTED:` FFI rows (`lib/task.f:107-112`, `TASK-NULL`
  etc.), but item 9's only stated task edit is the `CONSTRUCT` rename, which is a
  **no-op** (Contradiction C1: no `CONSTRUCT` word exists). So item 9 touches no
  task trust row.

**Result:** zero new trust rows is achievable and required —
`PLAN.md:989-992` "The type-family/ADT campaign may not add `TRUST`, `TRUSTED:`,
`set-check`, or `TRUSTED.md` rows". No gap.

---

## 6. PLAN item 9 acceptance restated as a checklist (files/words per criterion)

From `PLAN.md:673-683`:

1. **Private `construct family variant` resolves only inside the owning package,
   only through the checker-owned token protocol; bare variant words and external
   constructor words for private families do not resolve.** → new `construct`
   arm in `DO-TOK1` (`checker.f:4988`, before `LOC-REF?`) using `IS-NEXT-TOKEN`
   (`4939`) + `SUMV-FIND` (`type-family.f:368`) scoped by `CHECKER-PACKAGE-*`
   (`checker.f:3555` region); private = `TFAM-VIS@` (`type-family.f:196`)
   `CHECKER-PACKAGE-PRIVATE`.
2. **Exhaustive matches certify.** → `CF-MATCH` `;MATCH` walks
   `TFAM-VAR-START@`/`TFAM-VAR-COUNT@` (`type-family.f:201-202`) vs the seen
   bitset (spec §14 step 1-3, `docs/type-families.md:891-893`).
3. **Non-exhaustive / duplicate-variant / wrong-family / missing family token /
   missing variant token / default-branch syntax / branch-output mismatch /
   return-stack mismatch reject.** → `CF-MATCH` model + `SUMV-FIND`
   dup/family checks; branch-output via `SUNI`/`RSUNI` (`checker.f:4424,4439`);
   return-stack via `RSUNI`; surfaced through `render.f` `DCODE`/`REPAIR-CLASS`
   (`258/279`); "V1 has no default branch" (`PLAN.md:669-671`; spec `:900-902`).
4. **Linear-payload match fixtures reject until item 11, then prove exact branch
   consumption.** → hook to `LAYOUT-LINEAR?` (item 11, unbuilt; `PLAN.md:666-668`).
5. **Existing `CASE` fixtures continue to pass.** → `CF-OF`/`CF-ENDOF`
   (`checker.f:4506,4515`) must keep kind-7/8 CASE behavior while adding a
   `CF-MATCH` branch (Risk `PLAN.md:684-685`); parity gate
   `tools/compiler-dispatch-test.f` (CASE shape MUST-HAVE) unchanged.
6. **Reserved-name lint proves no pre-existing `CONSTRUCT` remains once
   `construct` is reserved; migrated task API keeps tests green.** →
   `tools/reserved-name-lint-core.f:107 RNL-RESERVED-CONTROL?` adds `construct`/
   `match`/`endmatch`. **Trivially satisfied — no `CONSTRUCT` word exists (C1).**

### Gate 17j checklist (item 17 applied to item 9, `PLAN.md:958-1027`)

- TDD red fixtures before code; owning suites `test/type-family-suite.f` +
  `test/type-family-rollback-suite.f` (`FILEMAP.md:691-692`) plus a new
  `MATCH`/`construct` behavior suite; JSON reject fixtures in
  `test/gate-diagnostics-lib.f` (census-7 §3d pattern).
- Rebuild `bin/hb`; native self-refresh/fixpoint byte-identical
  (`PLAN.md:995-997`); no-binary Gforth bootstrap reaches fixpoint.
- **Trust ratchet unchanged** (`PLAN.md:989-992`) — item 9 adds zero rows (§5c).
- New core file `src/core/match.f` ⇒ update `tools/srclist.f`, `FILEMAP.md`,
  `test/run-files.f` result-cache keys, `tools/hb-build-lib.f` ABI/source keys
  (`PLAN.md:968-978`) — none present today (§6 R2).
- Reserved-name fixtures for `construct`/`MATCH`/`;MATCH` in source-list files
  (`PLAN.md:434-436,500-501`); `filemap`/`host`/`public-signature` lints;
  candidate size vs `test/gate-build-size.f`.

---

## 7. Open risks / unknowns (each with a probe)

- **R1 — Tokens not reserved.** `: construct ( -- ) ;` and `: MATCH ( -- ) ;`
  both define at exit 0 today. *Probe:* re-run both after adding arms to
  `RNL-RESERVED-CONTROL?` (`tools/reserved-name-lint-core.f:107`) and to the
  parser; both must flip to `E-RESERVED-DEFINITION`.
- **R2 — No `CF-MATCH` storage / overflow policy.** `CFS` is a fixed 32-row
  array with an `UNCK` overflow (`checker.f:4349,4394`); PLAN needs a
  growable-or-diagnostic-reject frame (`PLAN.md:657-663`). *Probe:* count
  `CFS-REC` cells (`checker.f:4332-4347` = 14) and confirm the seen-bitset +
  per-arm rows fit an existing frame or need a sidecar arena; verify overflow
  rejects (not `UNCK`) with a >32-deep synthetic nesting.
- **R3 — Variant range unpopulated by `SUMV-ADD`.** *Probe (run):* after two
  `SUMV-ADD` calls, `TFAM-VAR-COUNT@` returned `0`; exhaustiveness must either
  call `TFAM-VAR-RANGE!` (`type-family.f:226`) in test setup or scan all SUMV by
  `SUMV-FAM@` (`356`). Decide and pin the enumeration source.
- **R4 — `OF`/`ENDOF` dispatch ambiguity (CASE vs MATCH).** `CF-OF`
  (`checker.f:4506`) does `STEP-N-IN` (consumes a scrutinee cell) — wrong for a
  MATCH arm (variant is a bare token, no cell popped). *Probe:* trace `CF-OF` for
  a `CF-MATCH`-topped frame; it must branch on `CF@K` before `STEP-N-IN`.
- **R5 — Match-mode capture ordering vs locals.** Variant tokens must beat
  `LOC-REF?` (`checker.f:4988`). *Probe:* a `MATCH` whose variant tail shadows a
  declared `{: ok :}` local — capture must resolve the variant, not the local.
- **R6 — Reject-only gating before item 12.** Full MATCH-over-layout can't lower
  until item 12 (`PLAN.md:663-665`). *Probe:* confirm the item-9 slice rejects a
  layout `MATCH` fail-closed (reuse the census-7 layout-reject seam
  `U-TYPE`/`CHECKER-STEP`) rather than emitting an unlowered body.
- **R7 — Payload refinement engine.** No SUMV-schema→row instantiation helper
  exists; `SC-QUOT` payloads are an open item-4 remainder. *Probe:* declare a
  synthetic sum with a `SCHEMA-APP` payload node (`type-schema.f:95`) and confirm
  `E-INST` (`checker.f:3017`)-style instantiation reproduces the payload row on
  DCUR at `OF`.

---

## Contradictions (PLAN/spec vs code)

- **C1 — PLAN says migrate a `CONSTRUCT` word in `lib/task.f`; no such word
  exists.** `PLAN.md:648-651` (and acceptance `:682-683`) require renaming "the
  pre-existing task API words `CONSTRUCT` (`lib/task.f`) and their call sites"
  because "the lowercase reservation collides with the uppercase definitions".
  But `lib/task.f` defines only `TASK-CONSTRUCTED` (constant, `:18`),
  `TASK-CONSTRUCTED?` (`:207`), and `PREPARE` (`:210`) — **no bare `CONSTRUCT`**
  (`rg '\bCONSTRUCT\b' src/ lib/ tools/ test/` = 0). Case-folded, `construct`
  collides with none of them (different spellings). **The migration is a no-op /
  stale reference** — item 9's `construct` reservation is safe as-is, but the
  implementer must not spend effort "migrating" a non-existent word. Flag for the
  orchestrator: either the PLAN references a since-removed word or an anticipated
  one. Not silently resolved.
- **C2 — Task prompt says `END-MATCH`; spec/PLAN/code use `;MATCH`.**
  `docs/type-families.md:802,885` and `PLAN.md:436` spell the terminator
  `;MATCH` (matching `ENDOF`/`ENDCASE`). Use `;MATCH`.
- **C3 — Item 9 Paths omit `src/core/match.f`, `tools/srclist.f`, `FILEMAP.md`,
  reserved-name-lint, cache keys.** Paths = `checker.f, lib/task.f,
  docs/type-families.md` (`PLAN.md:647`), yet Package Shape wants
  `src/core/match.f` (`:128`), the token reservation lives in
  `tools/reserved-name-lint-core.f`, and gate 17j requires
  `tools/srclist.f`/`FILEMAP.md`/`test/run-files.f`/`tools/hb-build-lib.f`
  updates for the new core file (`:968-978`). Expect edits well beyond the
  three named files (same incompleteness census-7 C2 / census-8 C4 flagged).
- **C4 — `CF-PUSH` overflow marks `UNCK`; item 9 must reject instead.**
  `checker.f:4394 #CFC @ 31 > IF -1 UNCK !` makes deep control nesting
  *uncheckable*; `PLAN.md:662-663` forbids that for `MATCH` ("Overflow must
  reject with diagnostics, not silently mark a definition uncheckable"). The
  `CF-MATCH` frame allocation must not reuse `CF-PUSH`'s `UNCK` path — a distinct
  fail-closed reject is required.
- **C5 — Dependencies 7/8/12 unbuilt; the "MATCH proper" slice is gated.**
  `PLAN.md:687` orders `7 -> 12 -> 8 -> 9`, but none are landed (§0). Item 9 is
  buildable only as reserve-tokens + `construct` resolution + `CF-MATCH`
  exhaustiveness on synthetic (friend-only) sums + reject-only layout gating;
  full MATCH-over-layout checking waits on item 12 (`PLAN.md:663-665`) and
  linear-payload matching on item 11 (`:666-668`). Scheduling reality, not a plan
  defect (mirrors census-8 C1).
- **C6 — Spec §6 vs code registry spelling / variant-range population.** Spec
  narrates a family record with `variant-start`/`variant-count`
  (`docs/type-families.md:295-296,311-312`) as if `SUMTYPE`/`SUMV-ADD` fills them,
  but in code `SUMV-ADD` (`type-family.f:375`) leaves `TF.VAR-START/COUNT` at 0;
  only `TFAM-VAR-RANGE!` (`:226`) sets them (probe: `TFAM-VAR-COUNT@`=0 post-add).
  Item 9's coverage enumeration must not assume the range is auto-maintained.
