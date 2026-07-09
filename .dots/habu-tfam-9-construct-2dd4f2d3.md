---
title: "TFAM 9: construct + MATCH token protocol + checking"
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-03T23:36:48.945599+02:00\""
---

PLAN.md item 9. Reserve construct/MATCH/ENDMATCH + branch tokens; construct family variant resolves (owning-package-id, family-id, variant-id) in owning package; match-mode token capture before dictionary lookup; growable CF-MATCH frames (family id, type args, base rows, seen variants, branch rows, dead paths, span) with fail-closed overflow; exhaustiveness (no default branch in v1); linear-payload matches reject until TFAM 11; CASE fixtures stay green. Gate 17j. Depends: TFAM 7, 8, 12.

## Audit refresh (2026-07-06, head 1eb3b5d3)

The "FIRST migrate lib/task.f CONSTRUCT" prerequisite is done: f968f1d93bad
"Rename task CONSTRUCT ahead of construct reservation" landed; no bare CONSTRUCT
token remains repo-wide (lib/task.f now uses TASK-CONSTRUCTED*). The reservation
and MATCH protocol work above is unstarted.

## Audit (2026-07-09, base 96052b3b "Close TFAM 12", fable-tfam12 lane)

### Deps satisfied
- TFAM 7 (hidden physical fields, logical-row expansion), 8 (generated
  constructors, slices 1..3b), 12 (layout-aware stack ops) all landed at base.
  `bin/hb` is the 8ce921d2 fixpoint; `bin/hb < test/type-ctor-suite.f` -> rc 0.
- No MATCH/construct scaffolding exists yet: `grep -rn` for CF-MATCH / "match" /
  "construct" tokens in src/core hits only USIG-MATCH-SYM?, DFER-MATCH-SYM?,
  render mismatch text — genuinely unstarted.

### Scope reconciliation (IMPORTANT — adapts the prompt's "expected shape")
Item 9 is a **checker-only** capability; the engine/native/Gforth *lowering* of
MATCH/OF/ENDOF/;MATCH and constructor tag pushes is **item 10** (PLAN lines
692-742: habu1.f/habu2.f/forth.fs keyword data, EMIT-KWDATA, label vars, bad-tag
die paths). Evidence the item-9 deliverable needs **zero engine codegen**:
- The checker scans a definition's raw source text token-by-token in
  `CHECK-SCAN`/`DO-TOK1` (checker.f ~6153-6261); control words are recognised in
  `CF-TOK?` (checker.f 5796-5821) purely on the token string, independent of the
  engine's `:` compiler. `MATCH result ok OF ... ENDOF ... ;MATCH` is just a
  token stream to the checker.
- Fixtures type-check through `CHECK-CANDIDATE!`/`CHECK-QUIET-CANDIDATE!`
  (checker.f 6505; test/checker-assert.f) which run `CHECK` inside a rollback
  frame **without compiling or running** the body — exactly item 8's
  type-ctor-suite pattern (`CHECK-QUIET-CANDIDATE! 0 T=` for check-only, keeping
  `INCLUDE-EVALUATE` only for real constructor words). No production code uses
  MATCH, so the engine `:` compiler never sees these tokens during
  fixpoint/gate; a real compiled MATCH word waits on item 10.
- check-core.f's preverify scanner (`CHK-NOM-STEP` 770-789) only registers
  top-level TYPEFAMILY/SUMTYPE and **skips `:` bodies** (`CHK-SKIP-DEF`), so
  construct/MATCH (body tokens) need no check-core change; they ride the normal
  checker path.
So slice (a) "engine keyword plumbing" reduces, for item 9, to **checker token
recognition + reserved-name/decl-keyword lists**; the habu2.f/forth.fs/stage0
mirror plumbing is deferred to item 10 (already its PLAN scope). This is a
deliberate, evidence-backed narrowing, not scope-shaving: attempting engine
codegen here would duplicate item 10 and cannot be gated (no runtime path).

### Design decision: block-pair spelling is `;MATCH` (not `ENDMATCH`)
The dot title says "ENDMATCH" loosely; the settled spelling is **`;MATCH`**:
- docs/type-families.md uses `;MATCH` uniformly (§13 line 823, §14 906, §23,
  §25.4 examples 1625/1635/1655/1665, diag §24 line 1555).
- PLAN pins it: line 701 "lowering for MATCH/OF/ENDOF/;MATCH", lines 437-438
  "item 9 reserves construct, MATCH, ;MATCH, plus branch tokens".
- Global convention (MEMORY): "Block pairs are FOO … ;FOO — never
  BEGIN-*/END-*/*-END".
Branch tokens are **`OF` / `ENDOF` shared with CASE** (docs §13/§25.4). Parser
dispatch distinguishes them by the enclosing control-frame kind (PLAN Risk line
686-687). No rename dot needed — `;MATCH` already matches the convention.

### Machinery found (what the slices build on)
- **Control frames** (checker.f 5410-5821): fixed 32-entry `CFS` arena, kinds 1
  if / 2 if+else / 3 begin / 4 begin+while / 5 do / 6 quotation / 7 case / 8
  case-of. `CF-PUSH` (5480) fail-closes overflow by `-1 UNCK !` (silent
  uncheckable) — item 9 forbids this for MATCH, so the CF-MATCH state must be a
  **growable side arena** (REG-GROW1 pattern), not a fixed CFS slot.
- **CASE** (`CF-CASE` 5591, `CF-OF` 5607, `CF-ENDOF` 5616, `CF-ENDCASE` 5626):
  reuses adjacent frames' CF.SB/RB as per-index accumulators and CF.DED as a
  seen flag. `CF-OF` requires enclosing `CF@K 7` (CASE); MATCH must branch here
  on a new MATCH kind so `OF`/`ENDOF` serve both without weakening CASE.
- **Token pipeline** `DO-TOK1` (checker.f 6153-6183): dispatch order is CAP-FAIL
  -> name-skip -> LIVE-TOKEN? -> LMODE(locals) -> `{:` -> UNSAFE -> `is` ->
  LOC-REF?(local) -> `CF-TOK?`(control) -> QDUP -> ... -> `DO-TOK`(word/prim/user
  sig). **Match/construct-mode capture must intercept BEFORE LOC-REF?/DO-TOK** so
  family/variant/branch tokens never resolve as locals or words (PLAN 660-661).
- **Registry** (type-family.f): `TFAM-RESOLVE`(288)/`TFAM-SIG-RESOLVE`(864,
  installed as TFAM-RESOLVE-XT) resolve a family in package scope; `TFAM-KIND@`
  `TFAM-SUM?`/`TFAM-ENUM?` `TFAM-ARITY@` `TFAM-VAR-START@/COUNT@` `TFAM-SLOTS@`
  `TFAM-WIDTH@`; `SUMV-FIND`(fam,name->id, 428) `SUMV-TAG@` `SUMV-PAYCELLS@`
  `SUMV-SCH-START@/COUNT@`. Checker reaches the registry only through installed
  friend XT cells (TFAM-ARITY-XT/-LAYOUT?-XT/-WIDTH-XT at 880-882) — item 9 adds
  the family-kind + variant-resolve + payload-instantiate friend XTs.
- **Layout substrate** (checker.f 943-1113, item 7/12): `LAYOUT-PARAM?`,
  `T-WIDTH`, `MK-HIDDEN`, `LAYOUT-PUSH-FIELDS`(1008), `PARAM-HID-OK?`; a resolved
  layout T-PARAM carries family args in PARAM>ARG; a concrete-arg family expands
  to W hidden physical fields (slot0..W-2 payload, W-1 tag) via PUSH-LOGICAL.
  MATCH pops these W cells, recovers args from the tag term's PARAM>ARG run, and
  per variant instantiates the payload schema with those args.
- **Linear guard** (checker.f 1047-1096): `LAYOUT-MAYBE-LINEAR?`,
  `LAYOUT-LINEAR?`, `LAYOUT-ARGS-OPEN?` — the same predicates gate "linear
  payload match/construct rejects until item 11".
- **Constructor effect** already computable: item 8's `CTOR-EXPECTED-ROW`(6315)
  builds `SGIN + (M-p pads + tag) type-n cells`, and the declared-hidden-field
  coercion publishes the layout bundle. `construct` reuses this shape inline
  (build the raw payload+pad+tag row from SUMV metadata, coerce to the family
  layout) — the arity>0 blocker from dot 8 (possibly-linear one-cell layouts)
  applies identically to `construct` of parametric families.
- **Diagnostics**: rejects flow through `CF-FAIL`(5507: OK=0, FAILSET) + span
  cells; rich MATCH text (§24: "missing variant err", "branch output mismatch",
  "ok branch leaves: ...") needs new reason cells rendered by render.f (DIAGXT).
- **Gate wiring**: five type suites in test/gate-engine-lib.f
  (GE-TYPE-FAMILY/DECL/CTOR/LINEAR/LAYOUT-SUITE, 347-401, run at 796-801). Gate
  17j adds `test/type-match-suite.f` + `GE-TYPE-MATCH-SUITE`.
- **Reserved-name surfaces**: tools/reserved-name-lint-core.f
  `RNL-RESERVED-CONTROL?` (107+) and sumtype.f `TDECL-CONTROL?`(93)/
  `TDECL-KEYWORD?`(86) list case/of/endof/... — add match/;match/construct so no
  family/variant/word can take those names. `CONSTRUCT` already migrated.

## Slice plan (each independently gateable; ONE COMMIT PER SLICE)

Full per-slice gate (docs/bootstrap.md): byte-fixpoint `bin/hb --load <libs>
tools/build-fixpoint.f tools/build-fixpoint-main.f -- all --force` byte-identical
proof; `bin/hb --load test/run.f` GATE_RC 0; the five type suites over stdin;
`bin/hb --load maki/test.f`; prop census (test/prop-test-core.f) classifying any
new prim; dot-dep-lint; typed-local-diff-lint on `jj diff --git`; TRUSTED.md
re-pin if lines shift. CASE fixtures proven green every slice.

### Slice 1 — reserved tokens + match/construct-mode capture (reject-only)
checker.f: `CF-TOK?` recognises `match`/`;match` (and routes `of`/`endof`/inner
tokens by frame kind); `DO-TOK1` gains match/construct capture state that
intercepts the family token after `MATCH`/`construct` and variant tokens before
`OF` **before LOC-REF?/DO-TOK**, so they never resolve as locals or words. In
this slice the captured MATCH/construct body still **rejects** with a named
diagnostic (reject-only scaffolding per PLAN 665-667) — the point is: tokens are
recognised (never "undefined word MATCH"), capture is proven, CASE untouched.
tools/reserved-name-lint-core.f + sumtype.f TDECL keyword lists gain
match/;match/construct. Fixtures (new test/type-match-suite.f, CHECK-CANDIDATE!):
`MATCH`/`construct` recognised not undefined; a bare variant word still
undefined; `: MATCH ( -- ) ;` / family named `match` rejected by lint. CASE
fixtures green. Acceptance: recognition + capture + reserved-name, no checking
yet.

### Slice 2 — `construct family variant` resolution + effect
checker.f construct capture resolves `(owning-package-id, family-id,
variant-id)` in the ACTIVE package via TFAM-RESOLVE (own private+public only)
and SUMV-FIND; builds the constructor step effect from SUMV metadata (payload
schema inputs consumed, family layout bundle produced — reuse item 8's
raw-row+hidden-field coercion) and applies it to DCUR/RCUR. Rejects: unknown
family, unknown/wrong-family variant, family not sum/enum, construct of a family
the active package cannot resolve (private in another package), and
linear/possibly-linear payload (LAYOUT-MAYBE-LINEAR? / open args) until item 11.
Arity>0 parametric families follow dot 8's blocker (one conservative logical
cell where args open; hidden-field bundle where args concrete). New friend XTs in
type-family.f: family-kind + variant-resolve + payload-instantiate. Fixtures:
private `construct foo bar` resolves only inside `package foo`; bare `bar` and
external ctor word do not; wrong/missing variant reject; linear payload rejects.

### Slice 3 — CF-MATCH frames + MATCH checking + exhaustiveness
Growable MF side-arena record: family-id, recovered arg terms, base data/return
rows, accumulated-output data/return rows (+has-output flag), growable
seen-variant bitset, payload slot count, dead-path state, source span. `MATCH
family`: resolve family (sum/enum), verify top cells are that family's layout
bundle, recover args, pop hidden fields, push MF frame + a CFS MATCH-kind frame
for nesting. `variant OF`: reject dup/wrong-family, mark seen, instantiate
variant payload schema with recovered args, set DCUR=base+payload, RCUR=base
return, push CFS of-body frame. `ENDOF`: accumulate live DCUR/RCUR into MF
output, mark dead, pop of-body frame. `;MATCH`: require all variants seen
(exhaustive; no default branch — reject default syntax), unify all live outputs,
set DCUR/RCUR = join, pop MF+CFS. Overflow: MF arena grows (never silent
uncheckable). Linear-payload MATCH rejects until item 11. Fixtures = docs §25.4
battery (accept exhaustive; reject non-exhaustive/dup/branch-join; generic third
family `packet`). CASE regressions green (shared OF/ENDOF).

### Slice 4 — diagnostics + negative battery + Gate 17j
render.f MATCH reason cells for §24 messages (unknown family, family mismatch,
unknown/duplicate variant, missing variant NAME, branch output mismatch,
return-stack mismatch, default-branch syntax, "expected sum or enum value on
stack"). Complete negative fixtures for every reject in item-9 acceptance. Wire
`GE-TYPE-MATCH-SUITE` into test/gate-engine-lib.f GE-CANDIDATE-VALIDATE (Gate
17j). prop census, dot-dep-lint, typed-local-diff-lint, TRUSTED.md/inventory
rows for any new TRUST site (none expected — checker is native primitives, not
TRUSTED: Habu). Close conditions per PLAN item-9 acceptance.

### Deferred to item 10 (NOT this dot): native/Gforth lowering of
MATCH/OF/ENDOF/;MATCH + constructor tag pushes + bad-tag die paths + the
runtime invalid-tag object/AOT test (§25.5). Item 9 fixtures stay check-only.

## SLICE 1 LANDED (commit "TFAM 9 slice 1: reserve match/construct tokens")

Refined slice-1 scope vs the plan above: this slice is the **reserved-name
reservation only** (definition-name surface); the match/construct-mode *capture*
folds into the construct (slice 2) and MATCH (slice 3) checker slices where the
tokens are actually handled — capture without handling would be dead scaffolding,
so it is deferred to where it is exercised. No throwaway.

Landed: tools/reserved-name-lint-core.f `RNL-RESERVED-CONTROL?` now reserves
`match` / `;match` / `construct` (case-folded) alongside case/of/endof, so no
`:`/`+:`/`create`/... definition may take those names (PLAN item-9 acceptance
"once `construct` is reserved"). `CONSTRUCT` was already migrated repo-wide
(f968f1d93bad); a fresh grep confirmed no source defines or tokenises
`match`/`;match`/`construct` outside comments, so the reservation adds zero new
violations. Fixture: tools/reserved-name-lint-test-lib.f gains a `control.f`
case (`: match` / `: ;match` / `: CONSTRUCT`) asserting E-RESERVED-DEFINITION +
`` `match` `` / `` `;match` `` / `` `CONSTRUCT` `` in the report.

Scope note: tool-only (reserved-name-lint-core.f is NOT baked into `bin/hb`
core), so no fixpoint rebuild is required — the gate confirmed
`candidate-build-skip=1` (fixpoint unchanged). TDECL family/variant-name
reservation (sumtype.f) and the checker keyword awareness ride the construct/
MATCH slices that rebuild the fixpoint anyway.

Gate tails (all green, base fixpoint 8ce921d2 unchanged):
- `bin/hb --load tools/reserved-name-lint-test.f` -> rc 0 ("reserved-name-lint-test: ok")
- `bin/hb --load test/run.f` -> `GATE_RC=0` ("PASS: native test suite (fixpoint
  + engine suite + checked hb + repl + hb-build) (9739ms <= 40000ms budget)";
  candidate-validate=1 runs all five GE-TYPE-*-SUITE)
- `bin/hb < test/type-{family,decl,ctor,linear}-suite.f` -> rc 0, "ok" each
- typed-local-diff-lint on `jj diff --git` -> rc 0
- dot-dep-lint -> "164 dot(s), 13 blocker(s), 0 finding(s)", test ok
No new prims (prop census unchanged); no new TRUST sites (TRUSTED.md unchanged);
maki/test.f unaffected (tool-only, not on maki load path).

## SLICE 2 LANDED (commit "TFAM 9 slice 2: construct form resolution + effect")

`construct family variant` is live in the checker: capture, resolution,
inline constructor effect, exhaustive negatives, engine fail-closed pin.

### Mechanism
- checker.f: `CONSTRUCT-FAM-XT`/`CONSTRUCT-STEP-XT` friend cells (next to the
  TFAM xt block); `CONM`/`CONFAM` mode machine + `CONSTRUCT-BEGIN`/
  `CONSTRUCT-TOK` just above DO-TOK1; DO-TOK1 dispatch inserts the CONM branch
  and `construct` recognition between LMODE and `{:` — BEFORE locals reference
  (`LOC-REF?`), control dispatch (`CF-TOK?`), and word lookup (`DO-TOK`), which
  is intentionally STRICTER than CASE (whose `of`/`endof` sit after LOC-REF?):
  PLAN 660-661 demands operand tokens never collide with locals or words.
  CHECK-RESET zeroes CONM; the unclosed-form check in CHECK and CHECK-DOES!
  gains `CONM @ 0 <>` so a truncated form (`construct` / `construct fam` at
  body end) hard-rejects. A failed family resolve poisons CONFAM (-1) and
  still consumes the variant token, so the three-token form always captures
  whole and an ownership/unknown-family reject stays verdict 0 (never blurred
  into uncheckable by the dangling operand hitting word lookup).
- type-family.f: `TFAM-ACTIVE-PKG$` (shared with sumtype.f, which drops its
  duplicate TDECL-PKG$); `TFAM-CONSTRUCT-FAM` = TFAM-FIND-IN(active-pkg, tail)
  + sum/enum kind gate; `TFAM-CONSTRUCT-STEP` = SUMV-FIND + the inline effect:
  TFC-MINT-VARS (one fresh checker var per family param, a..z cap parity),
  TFC-SCH-TERM (payload schema node -> term: param->minted var, con->MK-CON,
  ptr->recursed MK-PTR), TFC-PAY-ROW (din = fresh base row + payload terms in
  decl order), TFC-FAM-TERM (MK-PARAM over the minted vars via PARAM-SCR+),
  dout = famterm PUSH-LOGICAL, then `din dout CHECKER-STEP` — the SAME
  unification + DEXP/DACT diagnostics + linear snapshot/conservation as any
  word call. Installed via the two new xt cells at the end-of-file block.
- sumtype.f: TDECL-CONTROL? reserves construct/match/;match as family AND
  variant names (E-TDECL-NAME); TDECL-FAMILY uses TFAM-ACTIVE-PKG$.
- docs/type-families.md §12: checker-semantics paragraph pinning everything
  below.

### Design decisions (recorded per coordinator ask)
- **Ownership predicate = package identity.** The family must live in the
  ACTIVE checker package — `TFAM-FIND-IN(TFAM-ACTIVE-PKG$, tail)`, no public
  fallback, no qualified `PKG:family` operands (a colon token is not a
  registry tail; pinned rejecting). Cross-package construction never resolves
  even for PUBLIC families — those construct through their generated words
  (pinned accepting: CP3). Top level owns the global "" package, so top-level
  families construct at top level. Public families in their own package also
  construct (uniform predicate; docs frame construct as the private form, and
  nothing in PLAN/docs forbids owner-side use for public families — the
  effect is identical to the generated word by construction).
- **Operand folding**: family/variant tokens fold like every body token
  (TOKFOLD runs before dispatch), so `construct ZRES OK` == `construct zres
  ok`; registry tails are canonical lowercase. Pinned (CN2).
- **Linear semantics: generated-ctor PARITY, not blanket reject.** The
  coordinator's "reject linear payloads until TFAM 11 (LAYOUT-MAYBE-LINEAR?)"
  instruction is superseded by the tree: TFAM 11 slices 1+3 landed whole-
  bundle linear accounting (dot habu-tfam-11-linear-99fa9990; PARAM-ARG-LIN-
  BLOCK? was REMOVED there as wrongly rejecting the err/none mint), and
  test/type-linear-suite.f A1-A7 PIN that generated constructors consume
  linear payloads and mint the bundle with exact CHECKER-STEP conservation. A
  construct-only LAYOUT-MAYBE-LINEAR? reject would make the inline form
  strictly weaker than the equivalent generated word — a semantic fork with
  no soundness gain. construct therefore rides the SAME CHECKER-STEP
  accounting: consume/mint/padded accepts and reuse/loss/copy rejects are
  pinned as K1-K4/KR1-KR3 next to the A/R battery in the linear suite.
- **Interpret mode + compile mode are fail-closed by the engine, pinned in
  the gate.** `construct` is not an engine word: interpret-mode use dies
  `E-UNDEFINED: construct` rc 70 on stdin AND --load; a CERTIFIED construct
  body also dies the same way when the engine compiles it (checker certifies,
  compile fails closed) — that is the item-9/item-10 boundary, pinned by the
  new GE-CONSTRUCT-PENDING case in the engine runtime gate slice
  (test/gate-engine-lib.f, wired into GE-RUNTIME-CHECKS).
- **Fixture placement**: construct resolution/effect/ownership in
  test/type-ctor-suite.f (CN1-11, CB1-14, CP1-3); linear parity in
  test/type-linear-suite.f (K/KR); reserved-name declaration gates in
  test/type-decl-suite.f; engine fail-closure in test/gate-engine-lib.f.
  The dedicated match suite arrives with slice 3/4.

### Found + dotted (pre-existing, evidence in dot bodies)
- habu-def-compile-failure-7182eeb2: a definition whose ENGINE COMPILE fails
  inside `[: ... INCLUDE-EVALUATE ;] catch` CRASHES (SIGBUS register dump,
  rc 134) instead of throwing catchably; plain stdin/--load exit orderly 70.
  Repro needs no construct (`: X ( -- ) qwertyuiop ;` under TCE-CATCH).
- habu-interpret-err-under-8876b500: an INTERPRET failure inside the same
  boundary prints its diagnostic but catch returns 0 (swallowed).
  Consequence for suites: never TCE-CATCH a failing DEFINITION; construct's
  engine pin is a gate child-process case instead.

### Fixpoint + gate proofs (verbatim tails)
- rebuild: `bin/hb --load ... tools/build-fixpoint.f tools/build-fixpoint-main.f
  -- install --force` -> rc 0 "bin/hb refresh OK: compiler fixpoint";
  re-run `-- all --force` under the NEW engine -> rc 0 "bin/hb refresh OK:
  compiler fixpoint" (byte-identical product; sha256 d7485199095d7862...
  matches the gate's Habu-under-test line exactly).
- `bin/hb --load test/run.f` -> `GATE_RC=0`, "PASS: native test suite
  (fixpoint + engine suite + checked hb + repl + hb-build) (28307ms <=
  70000ms budget)", candidate-build-skip=0 (full candidate rebuild),
  candidate-validate=1.
- engine runtime slice standalone (HABU_UNDER_TEST=bin/hb, `-- runtime`):
  rc 0 incl. "PASS: construct checks; engine lowering stays fail-closed
  until item 10".
- `bin/hb < test/type-{family,decl,ctor,linear,layout-lower-pending}` -> rc 0
  "ok" x5 on the rebuilt engine.
- `bin/hb --load maki/test.f` -> rc 0 "test: ok" (device leg skipped
  off-device as designed).
- typed-local-diff-lint on the slice `jj diff --git` -> rc 0.
- dot-dep-lint -> "166 dot(s), 13 blocker(s), 0 finding(s)", test ok.
- No new prims (zero PRIM: rows in diff -> prop census unchanged; gate
  prop/debug phase PASS). No new TRUST/TRUSTED:/set-check sites (TRUSTED.md
  untouched; trust tool + lint manifest slices PASS).

REMAINING for item 9: slice 3 (CF-MATCH frames + MATCH checking +
exhaustiveness + linear-payload MATCH consumption rules) and slice 4 (rich
§24 diagnostics + negative battery + dedicated test/type-match-suite.f wired
as GE-TYPE-MATCH-SUITE = Gate 17j). The compiler-capture tuple for item 10
lowering (keyword data) is item 10 scope; the checker resolution records
(owning-package-id, family-id, variant-id) transiently in CONM/CONFAM + the
step.
