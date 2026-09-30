---
title: "Retire the checker symbols of unshipped records at capture"
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T16:49:49.729611+03:00"
---

Problem: `CHECKER-CAPTURE-PREPARE` (`checker.f:18203`) persists every symbol and every checker row. Measured on master's engine: 8,715 private symbols, 1,960 record-less globals, 281 record-less publics and 5 orphan FFI symbols cost 359,115 B of image (SYMS, SYM-STR, USIGS, NORETS, PES together), 62% of the per-word checker stores. The words they describe already have no record in the image.
Rule: the capture keeps a checker symbol, and everything keyed on it, only when one of these holds:
- a shipped named record resolves to it: a record in `[FIRST-CORE, ndict)` that is neither DNAME-INT nor package-private;
- it is a private symbol of a package that is not sealed;
- a primitive axiom (PES row) names it (21 are dictionary-absent);
- the image is the whitebox image, which keeps everything.
Acceptance:
- **Where the sweep runs:** a late file, `src/core/checker-surface.f` (after `xref.f` and `internal-mark.f` in `native-runtime.f`), installs the sweep into a new defer that `CHECKER-CAPTURE-PREPARE` runs ahead of `SYM-SNAPSHOT-PERSIST`. Its default is a no-op, like `REG-EXT-PERSIST-XT`, so the mid-load call in `LOAD-TARGET` (`native-build-core.f:229`) is untouched and the final one in `PREPARE-TARGET` (`:299`) sweeps.
- **Symbols:** dropped symbols are retired in place (`SYM-RETIRED`), so symbol ids stay stable. `HIDX-BUILD` skips retired rows, and the string pool is rebuilt from the kept rows.
- **Other rows:** `NORET-COMPACT` drops the rows of retired symbols, and `DFERS` and `UNSAFE-SYMS` are filtered.
- **Stale copy:** the stale PES boot copy (2,999 B) is zeroed.
- **Symbol-id readers:** those outside `checker.f` are enumerated (`sumtype.f:1273`, `structure-make.f`, `type-family.f`, `compiler/ir/build.f:1545` FSYM-ROWS, `compiler/native/frozen.f:65`, `lib/object-link.f:325`, `internal-mark.f`), and each is shown to name only kept symbols or to tolerate a retired one.
Files: `src/core/checker.f`, new `src/core/checker-surface.f`, `src/habu/native-runtime.f` (manifest row with its lint reason), new `test/checker-surface.f`.
Verify:
- A private helper of `checker.f`, and a DNAME-INT global, are E-UNDEFINED from user source, at check and at load.
- A public word certifies, and a wrong arity is E-MISMATCH.
- `defer`, `EXPORT`, `TYPED-VARIABLE`, STRUCTURE and `does>` work after boot.
- `tools/effect-store-census-run.f` shows no binding of a retired symbol.
- `test/run.f`; generations byte-identical; the data-table census DONE and SYM-STR-BOOT rows, before and after.
- SUITEs build-fixpoint-source, build-fixpoint-fixtures, certify-generated and aot-chain-producer pass on the swept product: they prove that product-hosted certification and refresh still work (certification re-interns the text's own private words, so no carve-out).
Depends: none for the record-less and whitebox rules. Private symbols are dropped once c550102f seals their packages; the rule reads the bit, so both can be built in parallel.
Parent: habu-ship-only-the-d7d38629. Design: the Fable surface design of 2026-09-30 (~/.cache/tmp/heron-arm64/design-surface.md); census: ~/.cache/tmp/heron-arm64/size-census/.
