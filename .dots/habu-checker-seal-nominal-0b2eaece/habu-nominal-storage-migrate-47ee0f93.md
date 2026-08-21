---
title: "Nominal storage: migrate CAD owners"
status: closed
priority: 1
issue-type: task
created-at: "2026-07-12T16:08:58.389635+02:00"
closed-at: "2026-07-18T14:56:57.479511+02:00"
close-reason: "Migration surface complete: FP-RID + FP-SP-NODE (fusion-plan.f) and LLA-IN-REF (lower-launch.f) to TYPED-BUFFER with typed readers; the 5 corruption seams rewritten typed (coverage preserved); raw-read/weakening/cross-family negatives added red. Scoping RCA: model-ir/tensor-value descriptor columns were ALREADY on the sealed LAYOUT-BUFFER facility (a retained capability per c5f44d66, not deprecated) and the handle pools landed typed in 488a4937 - not over-migrated per the acceptance line. TRUST 673->673: no row became structurally guaranteed (the RAW>*/*>RAW pairs are genuine index-refinement cast boundaries, re-audited 2026-07-18). LAYOUT-BUFFER correctly not retired (retained capability, many consumers)."
---

Problem: fusion, Model IR, tensor and lowering staging owners still use generic raw storage plus private nominal refinements. Acceptance: migrate FP-RID and nominal staging cells, Model IR descriptor columns, tensor-value descriptor columns and actual target/toolchain nominal storage to TYPED-VARIABLE or TYPED-BUFFER; counters/raw numeric columns stay generic; remove obsolete raw projections and test corruption seams; discharge corresponding TRUSTED rows; retire LAYOUT-BUFFER only after every consumer migrates. Files: maki/fusion-plan.f, model-ir.f, tensor-value.f, lower-launch.f and focused callers/tests, TRUSTED.md, docs. Verify: same-family round trips, target/toolchain, node/region, dtype/layout/address-space swap rejection, typed-local/trust lints, maki/full gates. Depends: habu-nominal-storage-typed-c5f44d66.

Claim (RELEASED 2026-07-18, merged): agent=nomstore workspace=.jj-ws/fable-nomstore

## RECORD 2026-07-18 (agent=nomstore, workspace .jj-ws/fable-nomstore)

Migrated the residual GENERIC-RAW nominal cells to the sealed generative facility
(`TYPED-BUFFER`, dot habu-nominal-storage-typed-c5f44d66). Model-IR and tensor
owner storage were already on the sealed facility before this lane — the
descriptor columns are `LAYOUT-BUFFER` (a RETAINED distinct sealed capability per
c5f44d66's "live numeric-result<nominal> LAYOUT-BUFFER positive unchanged"), and
the handle pools (MI-INOFF/MI-INS/MI-ROWS/MI-COLS/MI-IS-*, P-OUT/P-INS, TGT-SM87,
BW-CT/BW-ISG/BW-SEED, IMP-*, TRF-SRC) landed on `LAYOUT-BUFFER`/`TYPED-BUFFER`/
`TYPED-VARIABLE` in the sibling change "Migrate maki family-handle pools to typed
storage" (488a4937b5a7). tensor-value TV-DATA stays generic: it is a bare `ptr a`
data pointer, which `TYPED-BUFFER` rejects by design. Per the acceptance line's
"do not over-migrate" boundary, sound already-sealed LAYOUT-BUFFER columns were
NOT churned to TYPED-BUFFER (no soundness gain, and it contradicts c5f44d66's
retained-capability decision).

Migrated owners (raw `create`/`variable` holding a nominal family -> typed cell):
- maki/fusion-plan.f: `FP-RID` -> `FP-CAP TYPED-BUFFER FP-RID-AT CAD-KIND:region`
  (per-node region identity now a sealed typed cell). `FP-SP-NODE` ->
  `FP-CAP TYPED-BUFFER FP-SP-NODE-AT CAD-KIND:node-id` (per-split node id).
  New private typed reader `FP-RID-RGN`; `FP-RID-RAW` now reads the typed cell and
  projects through the audited `RGN>RAW` (was a confined generic-cell read = the
  seal invariant's forge vector). `FP-RID@` drops its `RAW>RGN` (region now read
  structurally typed); `RAW>RGN` is minted only at the store site (`FP-ADD`).
  `RAW>RGN`/`RGN>RAW` moved above `FP-RID-RAW` (they must precede it).
- maki/lower-launch.f: `LLA-IN-REF` ->
  `LLA-MAX-IN TYPED-BUFFER LLA-IN-REF-AT MIR:operand-ref` (per-input operand ref).
  The launch/toolchain nominal cells `LLA-OUT-NODE` / `MDL-PROBE-RID` are already
  sealed (`1 LAYOUT-BUFFER`); the CUDA device/ctx/module/function cells are raw
  driver handles, not CAD nominal families, so they stay generic.

Counters / genuinely-raw columns kept GENERIC (acceptance boundary): FP-MMC/RRC/
MEM/MIX/CAP-ROW, FP-RN/SP-N, LLA-IN-ELEMS/PM/PN/PK/CPA/CPB/NVAR, MDL-BUF/OWN/
CUBIN-LEN, MI-INCNT/ATTR/MAT/AD, P-INOFF/INCNT/ATTR, TV-HAS/DATA, all name/byte
buffers, and every count/flag/generation cell.

TRUST rows discharged: 0 (before 673, after 673). No refinement became
STRUCTURALLY guaranteed by the typed storage: the `RAW>NODE`/`NODE>RAW`,
`RAW>RGN`/`RGN>RAW`, `RAW>REF`/`REF>RAW`, `RAW>SLOT`/`SLOT>RAW`, `RAW>TENSOR`/
`TENSOR>RAW` (etc.) pairs are INDEX refinements/projections between raw table
positions (loop counters, allocator results) and nominal identities, plus the
`TYPED-LINEAR` eager-ABI adapter — all still required for validation, fact-array
indexing, and the `REGION_<rid>` render boundary regardless of storage typing.
Typed storage removes the CONFINED raw-cell reads (FP-RID-RAW / FP-SP-NODE@ /
LLA-IN-REF@ forge surface), not the cast boundaries. `RAW>RGN`/`RGN>RAW` audit
rows re-worded + re-audited 2026-07-18 (effects unchanged); no owner dot orphaned,
so no npol-style repoint needed.

LAYOUT-BUFFER verdict: NOT retired — extensive consumers remain and retirement is
also a src/core/layout-buffer.f change (out of this lane). It is a retained
distinct sealed capability (c5f44d66), and its own suite test/layout-buffer.f +
test/engine-suite.f + test/type-decl-suite.f + test/layout-valid-*.f exercise the
definer. maki consumers still on LAYOUT-BUFFER: report.f, mem-plan.f, async-dag.f,
cad.f, gradcheck.f, lower-ew.f, lower-mm.f, lower-move.f, lower-red.f,
target/target.f (TGT-DESCS), model-ir.f (MI-OP/ROWS/COLS/DT/LAY-AT, MI-IS-*-AT,
MIR-PROV-V, MIR-PEND-KIND), tensor-value.f (TV-ROWS/COLS/SPACE/DT/LAY/AL-AT,
P-KIND-AT, PEND-KIND), fusion-plan.f (FP-SP-REASON), lower-launch.f (LLA-OUT-NODE,
MDL-PROBE-RID); plus lib/build-cache.f, tools/hb-build-report.f, and many
tools/*-lint-test fixtures.

Negative coverage (item 5), each a red-today checked fixture:
- Region same-family round trip + node/region/plan swap rejection: pre-existing
  FPT-RGN-OK / FPT-NEG-RGN-AS-N/NODE/PLAN in fusion-plan-test.f (still green).
- FP-RID-AT (region) + FP-SP-NODE-AT (node) sealed columns: added FPT-RID-AT-OK/
  BARE/NOUT/ND/NIN and FPT-SPN-AT-OK/BARE/NOUT/RGN/NIN (positive certifies;
  raw-n read/store, bare `ptr a` weakening, and cross-family swap all reject).
- LLA-IN-REF-AT (operand-ref) sealed column: added LMT-INREF-OK/BARE/NOUT/ND/
  ST-OK/ST-NIN in lower-model-test.f.
- dtype/layout/address-space swaps: covered by the pre-existing LAYOUT-BUFFER
  descriptor-column negatives in model-ir-test.f / tensor-value-test.f (unchanged).

Test corruption seams: the FP-RID pokes in lower-mm-test.f (E-LMM-PROLOGUE,
E-LMM-MULTIMM) and lower-mv-test.f (E-MVW-STAGED x2, E-LMM-PROLOGUE) are RUNTIME
defense-in-depth tests — they store a VALID region into the wrong node slot, which
the type does NOT forbid, so the lowering analyzer's runtime guard is still the
check. Rewritten from `R N cells FP-RID + !` to the typed
`R FP-REGION-ID N FP-RID-AT !` (coverage preserved, not removed).

Gate table (all in .jj-ws/fable-nomstore, native ./bin/hb, HB_TMP=/tmp/hbtmp-nomstore):
| gate | result | exit |
| refine-lint | 60 mint(s), 0 finding(s) | 0 |
| maki-dep-lint | 448 file(s), 0 finding(s) | 0 |
| namespace-lint | 150 file(s), 0 finding(s) | 0 |
| host-lint | 0 finding(s) | 0 |
| filemap-lint | 925 path(s), 0 finding(s) | 0 |
| trust-lint | 691 site(s), 719 rows, 0 finding(s) | 0 |
| shadow-lint | clean (127 prims) | 0 |
| signature-lint (changed .f) | clean | 0 |
| error-code-lint | 0 finding(s) | 0 |
| stale-status-lint | 0 finding(s) | 0 |
| trusted-inventory --strict | 673 rows (baseline 673), separable 2/2 | 0 |
| typed-local-diff-lint (diff) | 0 finding(s) | 0 |
| fusion-plan-test.f (+new negs) | test: ok | 0 |
| model-ir-test.f | test: ok | 0 |
| tensor-value-test.f | test: ok | 0 |
| lower-model-test.f (+new negs) | test: ok | 0 |
| lower-mm-test.f (rewritten seam) | test: ok | 0 |
| lower-mv-test.f (rewritten seam) | test: ok | 0 |
| test/gate-stdlib.f | PASS native lint/stdlib phase | 0 |
| maki/test.f | 148 PASS, 0 FAIL | 0 |
