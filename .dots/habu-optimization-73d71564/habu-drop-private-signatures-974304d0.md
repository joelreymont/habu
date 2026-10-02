---
title: "Retire the checker symbols of unshipped records at capture"
status: closed
priority: 2
issue-type: task
created-at: "2026-09-16T16:49:49.729611+03:00"
closed-at: "2026-10-01T17:30:00.000000+02:00"
close-reason: "Landed: capture keeps only shipped-named, unsealed-private and axiom symbols; product 2,510,071 -> 2,344,951 B"
---

Problem: `CHECKER-CAPTURE-PREPARE` (`checker.f:18203`) persists every symbol and every checker row. Measured on master's engine: 8,715 private symbols, 1,960 record-less globals, 281 record-less publics and 5 orphan FFI symbols cost 359,115 B of image (SYMS, SYM-STR, USIGS, NORETS, PES together), 62% of the per-word checker stores. The words they describe already have no record in the image.
Rule: the capture keeps a checker symbol, and everything keyed on it, only when one of these holds:
- a record the image ships with its name resolves to it (capture's shipped-named set, DNAME-INT globals included: user lookup resolves them);
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
- A private of a sealed package and a private whose name the image strips lose their effect; a DNAME-INT global and a named private of an unsealed package keep theirs (`test/checker-surface.f`).
- A public word certifies, and a wrong arity is E-MISMATCH.
- `defer`, `EXPORT`, `TYPED-VARIABLE`, STRUCTURE and `does>` work after boot.
- Whitebox census window 4 in `test/effect-store-census-test.f` shows no binding of a retired symbol (`tools/effect-store-census-run.f` cannot load on the sealed product).
- `test/run.f`; generations byte-identical; the data-table census DONE and SYM-STR-BOOT rows, before and after.
- SUITEs build-fixpoint-source, build-fixpoint-fixtures, certify-generated and aot-chain-producer pass on the swept product: they prove that product-hosted certification and refresh still work (certification re-interns the text's own private words, so no carve-out).
Depends: none for the record-less and whitebox rules. Private symbols are dropped once c550102f seals their packages; the rule reads the bit, so both can be built in parallel.
Parent: habu-ship-only-the-d7d38629. Design: the Fable surface design of 2026-09-30 (~/.cache/tmp/heron-arm64/design-surface.md); census: ~/.cache/tmp/heron-arm64/size-census/.

Landed 2026-10-01: ARM64 product 2,510,071 -> 2,344,951 B (-165,120, 6.6%) against master 448ae659 built to its fixpoint; SYM-STR-BOOT 186,506 -> 75,063 B and DONE 443,806 -> 394,674 B of image (docs/engine-size.md, Retire unshipped checker symbols). Fable review: land; its three fold-ins are in (positive pin for an unsealed private, exact prelude band in ACAP-SHIPS-NAMED?, already-retired rows emptied). Two named records lost their effect, both privates of sealed packages; the four CLOSE-PRIVATE axiom rows of a sealed package stay by the axiom rule. Gate: build, generations 2-5 byte-identical, whitebox, 516 of 516 suites, dot lint. Left: retired SYMS rows and detached bindings are not compacted (habu-compact-the-effect-130fd5d0).

Follow-up landed 2026-10-02 (uqxmtupx e11128a5, Zero retired checker symbol rows): a retired SYMS row was VIS = -1, a ten-byte LEB128 DATA value per row for 11,082 rows; SYM-RETIRE now zeroes all five cells and SYM-INTERN refuses an empty name. Product 2,344,951 -> 2,229,367 B.
