---
title: "TFAM 15: product families + value-record/FIELD migration"
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-03T23:36:48.958843+02:00\""
---

PLAN.md item 15. FIRST migrate pre-existing FIELD words (lib/object.f:97, lib/object-test.f:28, src/habu/aot-lib.f:211 + call sites) before reserving PRODUCT/FIELD/END-PRODUCT (case-folded). Implement products after layout-aware ops proven; decide by evidence whether VALUE-RECORD becomes sugar or typed compat layer; migrate PTX IR only after by-value fixtures + no size regression. Gate 17o. Depends: TFAM 12 (order: after 14).

---

## AUDIT (2026-07-10, fable-tfam12 @ 28a96a09 / maki-type-families, TFAM 14 landed)

`docs/census-tfam-15.md` was written at TFAM item 4; its §0 "prerequisites
6/7/8/12 UNBUILT" claims are STALE. Tree is now at item 14: items 6,7,8,9,10,11,
12,13,14 are ALL landed. Re-audited against current source below. The census's
VREC/PF/FIELD *inventory* (§1-§6) remains accurate; only its "unbuilt" framing is stale.

### Naming decision — `PRODUCT ... FIELD ... ;PRODUCT` (NOT END-PRODUCT)
The dot says "END-PRODUCT"; that is WRONG. Both `docs/type-families.md` §9.4:544-547
and §26 Phase 7:1914-1918 spell it `PRODUCT name arity / FIELD f t / ;PRODUCT`, and
every landed precedent is `FOO ... ;FOO` (`;SUMTYPE`/`;VARIANT`/`;MATCH`/`;ENUM`).
DECISION: `PRODUCT name arity  FIELD f t  FIELD g u  ;PRODUCT`. Close token `;PRODUCT`.

### FIELD-word migration — PHANTOM, no rename needed (evidence)
- No bare `FIELD`/`field` word exists anywhere (`rg '^: FIELD |: field ...'` = 0).
- `src/habu/aot-lib.f:211` is `CALL-AT?` loop code; `rg -i '\bfield' aot-lib.f` = 0.
  The dot's aot-lib FIELD site is a phantom (census C1).
- The only FIELD* words are `FIELD+`(lib/object.f:97, object-test.f:28), `HASH-FIELD`,
  `HEX-FIELD`, `FIELD-BYTE`, `FIELD-CAP`, `FIELD$`, `OBJ:ROW-FIELD#/$` — ALL distinct
  tokens from bare `FIELD` (census C2).
- `tools/reserved-name-lint-core.f` matches EXACT tokens via `LINT-STR=CI`
  (e.g. `s" enum" LINT-STR=CI`), NOT prefix/substring. `FIELD+` != `field`.
- DECISION: reserve bare `field`/`product`/`;product` only; do NOT rename FIELD+ etc.
  Renaming distinct tokens is gratuitous churn with zero soundness benefit and the
  acceptance "reserved-name lint proves no pre-existing FIELD definition remains" is
  vacuously satisfied (there is none). The mandated "migrate FIELD words" precondition
  is discharged by proof-of-absence, matching census C1/C2's prediction.

### VALUE-RECORD verdict — TYPED COMPATIBILITY LAYER (evidence, airtight)
- Current fixtures `test/engine-suite.f:1027-1057` still pass with TOUCHABLE semantics:
  `T-POINT-X ( point -- n ) drop`(:1034) PASSES; `COK-POINT-DUP over over`(:1048) PASSES;
  `CBAD-POINT-DUP dup`(:1057) REJECTS. `FIELD-COERCE?`(checker.f:611-613) still coerces
  `field<>`->inner. Master is green => these demonstrably pass today.
- `field` wrapper family is `TK-CELL` (type-family.f:648), so `TFAM-LAYOUT?` is FALSE for
  it. Item 12's whole-bundle/hidden-field machinery keys on `TFAM-LAYOUT?*`
  (checker.f:950-967,1015) and therefore LEAVES `field<>` cells untouched BY DESIGN.
- A straight VALUE-RECORD->PRODUCT migration flips ~7 fixtures (census §3a): touchable
  `drop`/`nip`/`over over` become whole-bundle and `FIELD-COERCE?` rejects hidden fields.
- VERDICT: VALUE-RECORD stays a typed, tested, registry-backed (VREC) COMPAT LAYER with
  touchable TK-CELL `field<>` cells. PRODUCT is the NEW hidden-field product-family
  surface (TK-PRODUCT, layout-aware, untouchable). This is not just the correct choice —
  it is the CURRENT reality item 12 already established. Satisfies BOTH "existing
  value-record fixtures pass" AND "no size regression" (both N cells). Meets PLAN:930
  "compatibility acceptable only if typed, tested, registry-backed".

### PTX-IR migration verdict — DEFER + follow-up dot (evidence)
- `lib/ptx/ir.f:18` `ptxir-node` is a VALUE-RECORD; consumers `PTXIR-NODE-DROP`(5 raw drops),
  `PTXIR-NODE-DUP-RAW`(destructure+rebuild) rely on touchable cells (census §1i,§3a R2).
- PLAN:922 gates migration behind "fixtures prove by-value destructure AND no size
  regression". With VALUE-RECORD kept as compat, ptxir-node stays sound with ZERO risk;
  migrating it would force rewriting NODE-DROP/-DUP for no present consumer need (maki
  uses NO value records — census C6). PLAN item 16 follow-on names the real future
  consumer (maki Model CAD, dot habu-checker-capability-typed-a480c423).
- VERDICT: keep ptxir-node on VALUE-RECORD compat this item; DO NOT force the migration.
  Record a follow-up dot for the PTX-IR->PRODUCT migration when a consumer needs it.

### PRODUCT modeling — TK-PRODUCT + PF rows, k=0 constructor, field accessors
Registry substrate DONE (type-family.f): `TK-PRODUCT`(:16), `TFAM-PRODUCT?`(:217),
`TFAM-LAYOUT?` includes it(:220-221), `TFAM-WIDTH@` product = `TFAM-SLOTS@` (field cells,
NO tag)(:226-228), PF-* rows keyed by (fam,tail)(:392-447), `TFAM-FLD-RANGE!`(:238).
Layout expansion is ALREADY product-generic: `T-WIDTH`(checker.f:962-967),
`LAYOUT-PUSH-FIELDS`(:1015-1022), `PUSH-LOGICAL`(:2318) all drive off `TFAM-WIDTH@*`
+ `TFAM-LAYOUT?*` — no sum-specific tag assumption, so a TK-PRODUCT family expands to
W=fieldcount hidden fields for free.
- Grammar mirrors `sumtype.f` ENUM/SUMTYPE (buffer to `;PRODUCT`, TDECL-RUN transactional,
  reserved-name gate). Register TK-PRODUCT with arity N; parse `FIELD name type` pairs
  -> PF-ADD row (fam,name,schema-root,slot) + field schema in SCHEMA-ROOT pool; set
  `TFAM-SLOTS!` = total field CELLS (sum of field widths); NO variant/tag/SUMV rows.
- Constructor (item-8 path, k=0): `: PKG:tail ( f0sig f1sig .. -- tail<a,..> ) ;` with
  EMPTY body. `CTOR-PEND!`(checker.f:6635) with k=0 => CTOR-EXPECTED-ROW = SGIN (the N
  field cells) coerced to the product's W hidden fields. No pads, no tag literal.
- Field access: DESIGN FORK (record + recommend):
  (A) generated per-field accessor `PKG:tail-fname ( tail<..> -- fieldtype )` +
      a full destructure `PKG:tail> ( tail<..> -- f0 f1 .. )`. Needs a hidden->logical
      output coercion (LOGHID exists for ctor result; the reverse for accessor input).
  (B) MATCH-with-one-variant destructure (products as single-variant sums). Rejected:
      products have NAMED fields + no tag; forcing them through SUMV positional payloads
      loses field names and the PF registry the spec mandates.
  RECOMMENDATION: (A). PF rows carry field names/slots; accessor + whole-product
  destructure are the by-value surface PLAN:922 asks for. Constructor first (k=0, proven
  reusing TDGEN), then destructure `tail>`, then per-field accessors.

### Three parsers + no Gforth mirror (census R7)
`CHECKER-DEFRECORD` has 3 callers (roles.f, verify-source.f, check-core.f). A PRODUCT
grammar needs arms in: `src/core/sumtype.f` (engine definer, mirrors ENUM),
`src/habu/verify-source.f:529` `RECORD-DEFINER?` (preverify), `tools/check-core.f`
`CHK-NOM-STEP` (check tool). No Gforth/bootstrap mirror of records exists (census §1g);
grammar is a checker-prefix definer, so no `bootstrap/cg/*.fs` change unless a product
appears in the baked engine source (it does not).

## SLICE PLAN (one green commit each; byte-fixpoint x2 + full gate + type suites +
## maki/test.f + dot-dep-lint + typed-local-diff-lint + TRUSTED pins on every engine slice)
1. AUDIT + slice plan (this commit; docs/dot only — gate trivially green).
2. Reserve `product`/`field`/`;product` tokens: `sumtype.f` TDECL-KEYWORD? +
   `reserved-name-lint-core.f` RNL-RESERVED-DEFINER?. Reject fixtures:
   `TYPEFAMILY product`, `SUMTYPE field`, `ENUM ;product` reject as reserved; prove
   `: FIELD+`/`: HASH-FIELD` still PASS reserved-name-lint (distinct tokens). Green.
3. `PRODUCT ... FIELD ... ;PRODUCT` grammar (metadata-only, mirrors CHECKER-DEFSUM):
   register TK-PRODUCT + PF rows + field schemas + TFAM-SLOTS; rejects battery
   (dup field, empty product, bad/unknown field type, reserved family/field name,
   missing ;PRODUCT, uppercase name, package-visibility). Width + hidden-field
   expansion fixtures prove `pair<a,b>` signatures expand to W hidden fields. Add
   `verify-source.f` + `check-core.f` product arms. Green boundary: family registers,
   signatures type-check, rejects work; constructors NOT yet generated.
4. Constructor generation (k=0) via the engine `PRODUCT` word -> TDGEN no-tag path;
   `: PKG:pair` publishes + certifies. Positive+reject construction fixtures.
5. Destructure `PKG:tail>` + per-field accessors `PKG:tail-fname` (hidden->logical
   output coercion). By-value construct/destructure fixtures + linear payload (hdl-style)
   gated on item 11 rules.
6. Docs: `docs/type-families.md` §9.4 + `docs/effects.md` distinguish PRODUCT (hidden
   fields) vs VALUE-RECORD (touchable compat), resolving census C5 self-contradiction.
   `docs/census-tfam-15.md` §0 refreshed (prereqs built). VALUE-RECORD + PTX verdicts.
7. Gate 17o: filemap-lint covers docs; if a new `src/core/product.f` is split out,
   update `tools/srclist.f` + FILEMAP.md + build-cache keys + run-files result-cache;
   GE-CANDIDATE-SIZE-CHECK; no-binary Gforth bootstrap fixpoint. Trust ratchet UNCHANGED
   (zero new TRUST/TRUSTED:/set-check rows — the record path adds none, census §5).
8. Close with ledger; PTX-IR migration follow-up dot.

## PROGRESS

- Slice 1 LANDED (change 689c73de): audit + slice plan (this section above).
- Slice 2+3 LANDED (this commit): `PRODUCT name arity FIELD f t .. ;PRODUCT`
  engine grammar (src/core/sumtype.f) — registers TK-PRODUCT + PF field rows +
  per-field schema + TFAM-SLOTS(=field cells, no tag); reserves
  `product`/`;product`/`field` in TDECL-KEYWORD? and reserved-name-lint;
  `PRIM: PF-N@` exposure (checker.f) for the rollback baseline; product fixtures
  in test/type-decl-suite.f (registry shape, PF rows/slots/schema, width,
  hidden-field expansion via generic LAYOUT-PUSH-FIELDS, dup/drop transport,
  split/hidden-name rejects, full negative battery with PF-N rollback);
  pf-n@ added to the prim-axiom census (test/prop-test-core.f, AX-GEN difftested).
  Metadata-only: NO constructor generation yet.
  PROVEN GREEN: byte-fixpoint x2 (identical); type-decl/type-family/rollback/
  ctor/match suites ok; full gate (test/run.f) PASS; maki/test.f rc0;
  typed-local-diff-lint/host-lint/filemap-lint/dot-dep-lint clean; zero new
  trust rows; reserved-name-lint green (FIELD+/HASH-FIELD/etc. pass — distinct
  tokens, exact-match lint).
- DEFERRED to the constructor slice (NEXT): verify-source.f RECORD-PRODUCT +
  check-core.f product arm + `PRIM: CHECKER-DEFPRODUCT` exposure. Rationale: the
  preverify/check-tool arms are only exercised by a PRODUCT-using SOURCE FILE
  (none exists yet — the stdin type-decl-suite drives the engine definer
  directly), and adding the habu-layer reference forces a two-phase checker
  bootstrap (build-fixpoint `require`s verify-source.f against the running
  engine, which cannot see a brand-new CHECKER-DEFPRODUCT until it is baked).
  Matches the existing precedent that check-core.f has no ENUM arm either. These
  land WITH the constructor generation slice, when a PRODUCT-using file first
  needs them.

## ACCEPTANCE (PLAN:922-929 + Gate 17o:958-1021)
existing value-record fixtures pass (kept, compat verdict); product fixtures cover
by-value construct/destructure, hidden fields, logical rendering, package visibility,
linear payloads; reserved-name lint clean (bare FIELD reserved, distinct tokens pass);
docs distinguish supported vs legacy; zero new trust rows; native fixpoint byte-identical;
Gforth bootstrap reaches fixpoint; GE-CANDIDATE-SIZE-CHECK green; master advances only on
exact-tree green.

## LANDED (2026-07-10) — ITEM 15 COMPLETE

Commits (bookmark maki-type-families, workspace .jj-ws/fable-tfam12):
- `689c73de` (lyzvwsxy) TFAM 15: audit + slice plan (merged upstream pre-close).
- `310f7e71` (ktosptyx) TFAM 15: PRODUCT ... ;PRODUCT grammar + registration
  (merged upstream; slices 2+3: reservation + metadata grammar + decl fixtures).
- `1c0b17f6` (wwokwmtr) TFAM 15: product MAKE/UNMAKE generated words (slice 4a).
- `60283696` (wossutyo) TFAM 15: preverify + check-tool product arms (slice 4b).
- `035953ad` (zlkrxkkz) TFAM 15: docs - product surface + VREC compat verdict (slice 6).
- (this commit) TFAM 15: close dot — ledger.

What landed in `1c0b17f6` (5 files, +191/-7):
- src/core/sumtype.f: TDECL-PRODUCT-ROWS (two generator-owned SUMV rows `make`/
  `unmake` sharing the field-schema range, var-range + TDECL-CTOR-PUBLISH reuse);
  TDECL-PROD-WORD/TDECL-PROD-WORDS (render `: PKG:MAKE ( fields -- fam<..> ) ;`
  and `: PKG:UNMAKE ( fam<..> -- fields ) ;`, EMPTY bodies, k=0 CTOR-PEND
  window, CHECKER-RECORD-SYM into SV.CTOR-SYM); TDECL-CTOR-WORDS product branch;
  PRODUCT definer now calls TDECL-CTOR-WORDS.
- src/core/checker.f: `PRIM: CHECKER-DEFPRODUCT` (staged-landing enabler: bakes
  tool-source visibility so slice 4b compiles in ONE refresh from this commit's
  binary; prim-axiom census auto-classifies the `checker-` prefix NOEXEC).
- test/type-ctor-suite.f (+106): metadata rows/pkg/syms; checked MAKE/UNMAKE
  compositions; RUNTIME round-trips (ZPT-RT 3 4 -> 4,3; parametric ZPR-RT);
  user accessor compositions ZPT-X/ZPT-Y (UNMAKE + drop/nip) with runtime proof;
  payload rejects PB1-PB4 (wrong count/type, raw-forge, raw-split); PB5 bundle
  dup; PB6 construct kind-gate; ptr-field product; parametric MAKE/UNMAKE incl.
  generic wrappers (LOGHID both directions, U-ROW checker.f:1160) + PP1/PP2
  wrong-instantiation/cross-family rejects; PL1-PL3 linear fail-closed
  (construction, transport, destructure); in-package public + private-exports-
  nothing; E-CTOR-PROTECTED package-reopen/undefine with post-reject usability.
- test/type-decl-suite.f (+9): tdpair generated-row metadata asserts.
- test/type-match-suite.f (+10): GP1 MATCH-on-product E-MATCH-FAMILY-KIND, GP2
  construct-on-product E-CONSTRUCT-FAMILY-KIND (products are kind-gated out of
  both token forms).

What landed in `60283696` (3 files, +163/-18):
- src/habu/verify-source.f: PRODUCT-END? + RECORD-PRODUCT (metadata-only,
  mirrors RECORD-SUMTYPE) + RECORD-DEFINER? dispatch arm.
- tools/check-core.f: factored CHK-BLOCK-COLLECT (shared block-declaration
  collector; CHK-SUM-REGISTER rewritten onto it, dead CHK-SUM-END? removed);
  CHK-ENUM-REGISTER + CHK-PROD-REGISTER + CHK-NOM-STEP arms. The enum arm
  closes the item-14 gap (an enum-declaring file failed the nominal pass).
- tools/check-test-lib.f: CKT enum-good/enum-bad/product-good/product-bad
  fixtures + CKT-TEST-PRODUCT-ALL-ERRORS (proves verify-source support replay
  registers products: all-errors over PRODUCT + a using def -> rc 0).

Design decisions (evidence in AUDIT above + docs/type-families.md 9.4):
- Block form `PRODUCT ... ;PRODUCT` (docs 9.4 + 26, ;FOO convention); the dot's
  END-PRODUCT wording was wrong.
- FIELD-word "migration" discharged by proof of absence (census C1/C2): no bare
  FIELD exists anywhere; FIELD+/HASH-FIELD/etc. are distinct exact tokens under
  LINT-STR=CI; zero renames needed; reserved-name-lint green with `field`
  reserved.
- Generated surface = fixed tails PKG:MAKE/PKG:UNMAKE recorded as SUMV rows:
  reuses the ENTIRE item-8 publish/protection stack (ctor-pkg derivation,
  closed-but-callable WID wall, undefine guard, CTOR-SYM) with zero new
  protection machinery. Canon rules out `PAIR>`-style tails (`>` is not a canon
  tail byte); fixed generator-owned tails cannot collide with field names
  (fields generate no words).
- Destructure = UNMAKE under the same k=0 pending window (a product bundle IS
  its field cells in slot order; both directions are checker-owned metadata
  truth). No new checker machinery: parametric concrete-site calls ride the
  existing symmetric LOGHID row coercion.
- Per-field accessors are NOT generated: user compositions over UNMAKE
  (`ZPT:UNMAKE drop`) are fully checked and make linear-field access rejects
  automatic (effect taint on the dropped var). Fixture-proven at runtime.
- MATCH/construct kind gates untouched (products reject in both, fixtured);
  private products have no construction surface — documented fail-closed.
- VALUE-RECORD verdict: typed COMPAT LAYER (docs 9.4 + effects.md C5 fix); the
  touchable-vs-hidden fixture flip (census 3a/C4) decided it, and item 12 had
  already established the reality (the field family is TK-CELL, exempt from
  layout machinery).
- PTX-IR migration NOT forced: pre-existing dot habu-switchover-wave-d-1fcdef69
  updated with the R8 decision + landed-surface recipe; unblocked,
  consumer-driven. (A freshly minted duplicate dot was removed in its favor.)
- Rendering: product rejects render logical `rpt<>` (no @slot/field<> leak);
  the empty `<>` on arity-0 is shared pre-existing renderer behavior (enum and
  sum render identically) — parity, not an item-15 regression.

Proof (native macOS, workspace bin/hb fixpoint
sha256 320ca4c24b3734662a0da1fdf19a3a6ea4086fccd277ff4be5f2f821c98b88a9):
- Byte-fixpoint x2 at BOTH engine slices (4a and 4b): rebuild byte-identical.
- Staged landing proven live: 4a's binary compiles 4b's verify-source arm in
  one refresh (the parent binary could not — E-UNDEFINED CHECKER-DEFPRODUCT —
  which is why the PRIM row rides 4a). Two commits, each one-refresh buildable
  from its parent's binary.
- Full gate `bin/hb --load test/run.f` after 4a AND after 4b: rc=0 "PASS:
  native test suite (fixpoint + engine suite + checked hb + repl + hb-build)".
- Suites on the new engine: type-decl / type-ctor / type-match / type-family /
  type-family-rollback all "ok"; check-test rc=0 "check-test: ok" (new CKT
  product/enum/all-errors cases in the run).
- maki/test.f rc=0 after both engine slices. typed-local-diff-lint rc=0 on both
  diffs. host-lint 0 findings; filemap-lint 596 paths 0 findings; dot-dep-lint
  0 findings. No new TRUST / TRUSTED: / set-check / TRUSTED.md rows.

Remaining (tracked elsewhere, none blocking item 15):
- habu-switchover-wave-d-1fcdef69: ptxir-node -> PRODUCT when a consumer needs
  it (unblocked, recipe recorded).
- PLAN item 16 (POLICY) is the campaign's next and final item (Gate 17p after).
