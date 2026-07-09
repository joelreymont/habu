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

## ACCEPTANCE (PLAN:922-929 + Gate 17o:958-1021)
existing value-record fixtures pass (kept, compat verdict); product fixtures cover
by-value construct/destructure, hidden fields, logical rendering, package visibility,
linear payloads; reserved-name lint clean (bare FIELD reserved, distinct tokens pass);
docs distinguish supported vs legacy; zero new trust rows; native fixpoint byte-identical;
Gforth bootstrap reaches fixpoint; GE-CANDIDATE-SIZE-CHECK green; master advances only on
exact-tree green.
