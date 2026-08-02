# Unified Type DSL Hard-Cutover Census

**Snapshot:** `master` at `beb9ca05`  
**Cutover parent:** `habu-epic-one-structure-04f9804f`

This census is the migration contract for replacing every public composite or
type-family declaration block with exactly three declaration words:

```forth
STRUCTURE name arity ... ;STRUCTURE
ENUM name arity ... VARIANT ... ;VARIANT ... ;ENUM
ENUM name variant ... ;ENUM
NEWTYPE name arity
```

`NEWTYPE` is permanent. Joel ruled on 2026-07-26 (dot
`habu-rename-typefamily-definer-538979cc`) that the nominal wrapper definer is
the third and last declaration word, ratified alongside `STRUCTURE` and `ENUM`,
and renamed from its former spelling `TYPEFAMILY` in the same breath: in type
theory a type family is a type-level function, whereas this declaration is
exactly a Haskell newtype — a zero-cost, nominally distinct wrapper over one
cell, and with a private mint, a newtype with a hidden constructor. The rename
was hard: no alias, no compatibility definer, and the retired spelling is not a
word (the tombstone regression lives in `test/type-decl-suite.f`). A single-cell
nominal role therefore keeps its own short declaration instead of being written
as a field-less `STRUCTURE`.

There are no aliases, compatibility definers, or mixed-syntax transition
forms. `PRODUCT`, `VALUE-RECORD`, `BEGIN-STRUCTURE`,
`END-STRUCTURE`, `SUMTYPE`, and their closing/field words are removed after
their owning migrations land. `ENUM+` and `ENUM4+` are also retired: typed
named alternatives use `ENUM`; unrelated integer constants use `constant`.

## Static declaration baseline

The baseline counts top-level declarations in tracked `.f`/`.fs` sources under
`src/`, `lib/`, `maki/`, `tools/`, `test/`, `bootstrap/`, and `examples/`.
Generated-source strings and token-recognition code are inventoried separately.

| Removed surface | Declarations | Files | Migration owner |
|---|---:|---:|---|
| `BEGIN-STRUCTURE ... END-STRUCTURE` | 20 | 6 | `habu-migration-core-records-77182600`, `habu-migration-libs-to-4e798110` |
| `VALUE-RECORD ... END-VALUE-RECORD` | 10 | 3 | `habu-migration-tests-and-51d00332`, `habu-migration-maki-models-c965e65d` |
| `PRODUCT ... ;PRODUCT` | 30 | 12 | `habu-migration-libs-to-4e798110`, `habu-migration-tests-and-51d00332`, `habu-migration-maki-models-c965e65d` |
| `SUMTYPE ... ;SUMTYPE` | 75 | 17 | `habu-migration-libs-to-4e798110`, `habu-migration-tests-and-51d00332`, `habu-migration-maki-models-c965e65d` |
| legacy payloadless `ENUM ... ;ENUM` | 37 | 21 | `habu-migration-libs-to-4e798110`, `habu-migration-tests-and-51d00332`, `habu-migration-maki-models-c965e65d` |
| `ENUM+` / `ENUM4+` counter definers | 2 | 1 | `habu-type-dsl-delete-8bd73b41` |

Reproduce the baseline with:

```text
rg -ni '^[[:space:]]*BEGIN-STRUCTURE[[:space:]]' src lib maki tools test bootstrap examples --glob '*.f' --glob '*.fs'
rg -ni '^[[:space:]]*VALUE-RECORD[[:space:]]' src lib maki tools test bootstrap examples --glob '*.f' --glob '*.fs'
rg -ni '^[[:space:]]*PRODUCT[[:space:]]' src lib maki tools test bootstrap examples --glob '*.f' --glob '*.fs'
rg -ni '^[[:space:]]*SUMTYPE[[:space:]]' src lib maki tools test bootstrap examples --glob '*.f' --glob '*.fs'
rg -ni '^[[:space:]]*ENUM[[:space:]]' src lib maki tools test bootstrap examples --glob '*.f' --glob '*.fs'
rg -ni '^[[:space:]]*:[[:space:]]+ENUM[4]?\+[[:space:]]' src lib maki tools test bootstrap examples --glob '*.f' --glob '*.fs'
```

The last command counts the two defining words. Inventory generated source and
negative fixtures separately; those matches do not change the declaration
count:

```text
rg -ni 'ENUM[4]?\+' src lib maki tools test bootstrap examples --glob '*.f' --glob '*.fs'
```

## Declaration sites

### Pointer-layout structures

- Core registry records: `src/core/checker.f`, `src/core/type-family.f`,
  `src/core/type-schema.f`.
- Library records: `lib/vector.f`, `lib/task.f`, `lib/ptx/ir.f`.
- Composite definition/effect owners and generated fixtures:
  `src/core/structures.f`, `src/core/structures-effects.f`,
  `test/gate-dictionary-lib.f`, `test/internal-word-gate.f`.
- Pointer-storage definition/effect owner and focused regression:
  `src/core/pointer-storage.f`, `src/core/pointer-storage-effects.f`,
  `test/pointer-storage-test.f`.

`habu-migration-core-records-77182600` owns core declarations and the removal
of hand-threaded byte offsets from core clients.
`habu-migration-libs-to-4e798110` owns library declarations.
`habu-migration-tests-and-51d00332` owns generated/test-only declarations.

### Value records and products

- Value records: `src/core/roles.f`, `maki/schedule.f`,
  `test/engine-suite.f`, `test/type-decl-suite.f`, plus recognizers and
  generated fixtures in `src/core/sumtype.f`, `src/habu/verify-source.f`,
  `tools/check-core.f`, `tools/check-all-errors-test.f`,
  `tools/check-test-lib.f`, `tools/public-signatures-test.f`, and
  `test/internal-word-gate.f`.
- Product declarations: `lib/ptx/ir.f`, `maki/async-dag.f`, `maki/model-ir.f`,
  `maki/sched-key.f`, `test/layout-buffer.f`,
  `test/layout-valid-product-bad.f`, `test/lower-cert.f`,
  `test/type-ctor-suite.f`, `test/type-decl-suite.f`,
  `test/type-family-suite.f`, `test/type-match-suite.f`, and
  `tools/public-signatures-test.f`.
- Product token owners: `src/core/sumtype.f`, `src/habu/verify-source.f`,
  `tools/bootstrap-mirror-lint.f`, `tools/check-core.f`,
  `tools/check-test-lib.f`, `tools/public-signatures-core.f`, and
  `tools/public-signatures-test.f`.

Shared syntax events, field/variant transactions, and product/value-record
metadata converge under `habu-type-dsl-unify-b65d46c1`. Generated checked
structure operations land under `habu-type-dsl-implement-50f8dc15`. Source
migrations remain owned by the directory-specific migration dots above.

### Cell families, sums, and enums

- Cell-family declarations: `maki/async-dag.f`, `maki/cad-kinds.f`, `maki/model-ir.f`,
  `maki/tensor-value.f`, `test/layout-buffer.f`,
  `test/type-ctor-suite.f`, and `test/type-decl-suite.f`.
- Sum declarations: `lib/adt/option.f`, `lib/adt/result.f`, `lib/map.f`,
  `lib/process.f`, `maki/target/target.f`,
  `test/bootstrap-wide-interpret-src.f`,
  `test/bootstrap-wide-memory-src.f`, `test/bootstrap-wide-tick-src.f`,
  `test/engine-suite.f`, `test/layout-buffer.f`, `test/lower-cert.f`,
  `test/type-ctor-suite.f`, `test/type-decl-suite.f`,
  `test/type-family-suite.f`, `test/type-layout-lower-pending.f`,
  `test/type-linear-suite.f`, `test/type-match-suite.f`, and
  `tools/public-signatures-test.f`.
- Enum declarations: `lib/map.f`, `maki/async-dag.f`, `maki/fusion-plan.f`, `maki/model-ir.f`,
  `maki/op-kind.f`, `maki/report.f`, `maki/report-test.f`,
  `maki/sched-key.f`, `maki/tensor-value.f`, `maki/tensor.f`,
  `test/layout-buffer-depth-7.f`, `test/layout-buffer.f`,
  `test/layout-valid-growth.f`, `test/layout-valid-guard-base.f`,
  `test/layout-valid-product-bad.f`, `test/layout-valid-w1-bad.f`,
  `test/lower-cert.f`, `test/type-decl-suite.f`,
  `test/type-family-suite.f`, `test/type-match-suite.f`, and
  `tools/public-signatures-test.f`.
- Generated declarations and parser/checker/diagnostic owners additionally
  occur in `src/core/checker.f`, `src/core/render.f`,
  `src/core/sumtype.f`, `src/core/type-family.f`, `src/habu/aot.f`,
  `src/habu/habu2.f`, `src/habu/verify-source.f`,
  `tools/bootstrap-mirror-lint.f`, `tools/bootstrap-mirror-lint-test.f`,
  `tools/check-all-errors-test.f`, `tools/check-core.f`,
  `tools/check-test-lib.f`, `tools/gate-json-assert-core.f`,
  `tools/public-signatures-core.f`, `tools/repair-packet-test.f`,
  `tools/repair-schema-doc-test.f`,
  `tools/reserved-name-lint-test-lib.f`, `test/export-package.f`, and the `test/gate-*`,
  `test/lower-txn-*`, `test/seal*`, and `test/wide-store-seal.f` fixtures.
- Numeric counter definers exist only in `src/core/enums.f`; their only live
  consumer is generated test source in `test/gate-dictionary-lib.f`. The delete
  dot removes the file, source registries, effect rows, and tests. It migrates
  typed names to the unified `ENUM` and unrelated integer sequences to named
  `constant` declarations.

`habu-type-dsl-unify-b65d46c1` owns the shared parser events and declaration
transaction. `habu-type-dsl-implement-a762cfaf` owns enum-specific validation
and the generated variant surface. `habu-checker-certify-unified-5d56fe73` owns
checker certification and diagnostics. `habu-compiler-lower-unified-5f599080`
owns compiler capture, lowering, snapshots, AOT metadata, and recovery parity.

## Shared metadata and generated packages

The cutover reuses one schema graph for both blocks:

- family and layout rows: `src/core/type-family.f`;
- field/variant schema nodes: `src/core/type-schema.f`;
- declaration transactions and generated packages: `src/core/sumtype.f`;
- checker expansion, hidden fields, matching, and stored effects:
  `src/core/checker.f` and `src/core/render.f`;
- source replay: `src/habu/verify-source.f`, `tools/check-core.f`, and
  `tools/check-all-errors-*`;
- compiler capture/lowering: `src/habu/habu2.f`, `src/habu/aot.f`, and the
  bootstrap mirror;
- protected generated wordlists, snapshots, and AOT persistence:
  `src/core/type-family.f`, `src/habu/snap-*`, `src/habu/aot-*`, and their
  build/source registries.

The unified field key is `(family-id, optional-variant-id, field-tail)`.
Structure rows carry no variant id; enum payload rows carry their variant id.
Rows preserve declaration slot, schema root, width, alignment, byte offset,
visibility, and source span. Constructor inputs, `UNMAKE`, and `MATCH` bindings
remain in declaration order while reflection gains exact field names.

The generated package remains the only callable construction surface. A public
field-bearing `STRUCTURE point ...` publishes `POINT:MAKE`, `POINT:UNMAKE`, and
typed field operations. A zero-field `STRUCTURE` is an opaque one-cell family
and publishes no raw constructor. A public `ENUM message ...` publishes one constructor per variant
(`MESSAGE:QUIT`, `MESSAGE:MOVE`, and so on). The package is closed after
generation. Private declarations expose the same operations only to their
owning package; they do not publish a public constructor package.

Package spelling keeps the existing injective uppercase escape/join algorithm;
the cutover does not rename callable constructors or persisted WIDs. Field
accessors use exactly `FAMILY:FIELD ( ptr family<a,...> -- ptr field-type )`.

The early-load cycle at `src/habu/habu2.f`, `bootstrap/cg/forth.fs`, and
`tools/bootstrap.sh` is owned by `habu-migration-core-records-77182600`.
Pre-checker records use explicit named offsets, strides, ordinary accessors, and
load-time offset/size/alignment/pointer-role assertions. They have no parser,
definer, family metadata, descriptors, adoption phase, snapshot rows, or AOT
rows. Native and recovery layouts must be exact mirrors.

Both load paths use this order: utilities, checker private layouts,
lower-certificate base, type-schema private layouts, type-family private
layouts, render support, checker hook, unified `STRUCTURE`/`ENUM`, then the
remaining core. `habu-type-dsl-implement-50f8dc15` owns the sole post-hook
composite declaration parser; `habu-compiler-lower-unified-5f599080` mirrors
only that final checked language.

`habu-type-dsl-unify-b65d46c1` owns the shared schema and transaction model.
`habu-type-dsl-delete-8bd73b41` owns deletion of duplicate registries,
definers, keywords, generated-word paths, and stale snapshot/AOT rows after all
consumers migrate.

## Hard-cutover completion

`habu-type-dsl-enforce-19a93c1a` adds the zero-occurrence lint and error-only
retired-token table. Removed tokens are not executable words and never dispatch
to compatibility code. `habu-type-dsl-prove-93da83c4` owns native fixpoint,
bootstrap recovery, snapshot/AOT parity, exact diagnostics, focused unchecked-boundary tests,
the full native gate, and the final census assertion.

Completion requires zero live executable or generated-source occurrences,
except the error-only tombstone table and explicitly allowlisted negative
fixtures, of:

```text
BEGIN-STRUCTURE END-STRUCTURE +FIELD PTR-FIELD: CFIELD:
VALUE-RECORD END-VALUE-RECORD
TYPEFAMILY PRODUCT ;PRODUCT SUMTYPE ;SUMTYPE ENUM+ ENUM4+
```

`TYPEFAMILY` reaches zero by rename rather than by deletion: the definer it
named is retained as `NEWTYPE`, and the retired spelling is now nothing at all.
That half of the completion contract is already discharged — the whole tree
carries no `TYPEFAMILY` outside the tombstone regression in
`test/type-decl-suite.f` and the historical dot and lessons records, which are
records and are not rewritten.

`VARIANT`, `;VARIANT`, `FIELD`, `ENUM`, `;ENUM`, and `NEWTYPE` remain as parts
of the new grammar. The final lint distinguishes those legal contexts from every
legacy form and proves every exception is non-executable test data.
