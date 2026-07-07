---
title: "Maki: subsystem packages + maki.f re-export"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T01:10:04.572860+02:00"
---

Replace stem-prefix namespacing (gpu-*, eval-*, loss-*, GRADE-*, file-name stems) with real packages per docs/forth.md:70-89. One package per subsystem inside maki: TENSOR, ARRAY, LOSS, OPTIM, AUTOGRAD, TRAIN, GPU, EVAL, ONNX, FUSION (+ CAD/REPORT from cad-0a/0b when they land). Word names lose their stems (LOSS:NLL not GAUSSIAN-NLL-LOSS); file names lose stems where the package makes them redundant. Top-level maki.f re-exports the curated public interface via the compiler EXPORT capability (depends: habu-compiler-pkg-re-688212c1) so users load maki.f and call MAKI:* or subsystem packages directly. Migrate leaf-first, module-by-module, callers updated in the same increment, maki gate (bin/hb --load maki/test.f) green each step. Supersedes habu-roll-out-the-3bd15be6 (single flat MK: namespace) - close or fold that dot into this one. E-* error constants stay cross-cutting. Add the namespace lint idea from the old dot: flag top-level maki definitions outside a package.

UPDATE 2026-07-07 (TFAM 1-8 landed; this lane is MOSTLY pre-unlock executable).
Studied the landed surface + the fable-side maki files. Findings:

WHAT THIS LANE ACTUALLY DEPENDS ON: only `package`/`end-package` (landed long
ago) for the per-subsystem packaging, and the compiler EXPORT capability
(habu-compiler-pkg-re-688212c1) for the ONE top-level maki.f curated re-export.
It does NOT depend on any TFAM 9/10/12/14/15 ADT runtime item.

PRE-UNLOCK: the leaf-first package split + stem-strip is executable NOW,
module-by-module, without waiting on the type-family campaign. Proof from the
tree: maki/report.f, maki/sched-key.f, maki/store.f already open `package MAKI
... public/private ... end-package` today. So the real work is re-splitting the
single flat MAKI package into per-subsystem packages (TENSOR, ARRAY, LOSS,
OPTIM, AUTOGRAD, TRAIN, GPU, EVAL, ONNX, FUSION, + CAD/REPORT), dropping word
stems (LOSS:NLL not GAUSSIAN-NLL-LOSS) and file stems where the package makes
them redundant, callers updated in the same increment, `bin/hb --load
maki/test.f` green each step. The ONLY step that must wait is the top-level
maki.f re-export of the curated public interface — that needs EXPORT (spec 2).
Start the subsystem packaging now behind spec 2; wire maki.f last.

ORDERING: spec 2 (EXPORT) before this lane's maki.f step only. Everything up to
maki.f (the subsystem packages themselves) can proceed in parallel with the
TFAM campaign.

FOO/;FOO CONFORMANCE: `package`/`end-package` is the established package scope
pair (docs/forth.md §Scope pairs treats it as conformant; `end-package` is the
sanctioned closer). This lane introduces no NEW scope words — no `;FOO` renames
owed. The namespace lint should also flag any legacy `BEGIN-`/`END-` pairs it
uncovers while moving files, but that is reporting, not new scope words.

COLLISION MAP: this lane touches only fable-side maki/*.f + maki/test.f + the
new namespace-lint tool. NO overlap with the TFAM campaign's src/core/* /
src/habu/* files. Clean vs the campaign. Its only cross-lane dependency is spec
2 (EXPORT) for the maki.f re-export tail. Supersedes habu-roll-out-the-3bd15be6
(single flat MK: namespace) — close or fold it. E-* error constants stay
cross-cutting (they are already top-level `constant` defs above the package
blocks, e.g. E-SK-*/E-RPT-*/E-STORE-*; the namespace lint must whitelist those).

---

PROGRESS 2026-07-07 (session 1, workspace .jj-ws/fable-pkgs, host-side).

3 COMPLETE clean clusters landed, each its own commit, `bin/hb --load
maki/test.f` green (73/73 suites, 0 fail) AFTER EVERY commit; `test/run.f` green
at start/middle/end (native suite ~6.6s, budget 40s); `typed-local-diff-lint`
clean on the full 3-commit diff. No bookmark moves, no push. Commits (on top of
`fable` base tqupyqqr):
  - xquppyro  maki: extract ONNX subsystem into package ONNX
  - kzumxsnv  maki: extract LOSS subsystem into package LOSS
  - pwkmtvrm  maki: extract OPTIM subsystem into package OPTIM

PER-CLUSTER DECISIONS + call-site data:
  ONNX (onnx.f -> package ONNX): ONNX-LOWER->ONNX:LOWER, ONNX-MOVE-KIND->
    ONNX:MOVE-KIND (stem stripped; package carries "ONNX"). Downward: op-kind
    FACTs qualified MAKI:OP-RESHAPE... (op-kind still MAKI, public). Callers:
    onnx-test.f (13 sites) + docs/maki/onnx.md prose. Only 1 external caller
    file — the model ONNX-clean extraction.
  LOSS (loss.f + loss-tensor.f + celoss.f -> package LOSS): scalar names ALREADY
    clean, KEPT verbatim (MSE, NLL, HUBER, MAHALANOBIS, L1, RES2, VAR-OK, the
    *-GRAD family) — the dot's "LOSS:NLL not GAUSSIAN-NLL-LOSS" is already the
    spelling on disk, so no churn there. STRIPPED redundant `-LOSS` suffix from
    the tensor forwards: TT-MSE-LOSS->TT-MSE, TT-NLL-LOSS->TT-NLL,
    TT-MAHALANOBIS-LOSS->TT-MAHALANOBIS, TT-HUBER-LOSS->TT-HUBER (gradients keep
    -DY/-DMU/-DLV). CE-LOSS->CE. KEPT SOFTMAX-CE-BWD (names the fused method, not
    redundant with LOSS). Downward qual: only FEXP/FLN -> MAKI:FEXP/MAKI:FLN
    (fmath is real `package MAKI`); T-GET/T-SET stay BARE (array is GLOBAL — see
    finding below). Callers updated: train.f (LOSS:MSE, LOSS:MSE-GRAD),
    from-scratch-train.f (LOSS:NLL/-MU-GRAD/-LOGVAR-GRAD), mlp-test.f, celoss-
    test.f, loss-test.f (25 code sites), loss-tensor-test.f (18 sites). All
    callers were `package MAKI`. FALSE-POSITIVE avoided: from-scratch-test.f
    `SCT-L1` is NOT loss L1; comment-only MSE/NLL mentions in
    autograd-tensor-test/train-test/gpu-train NOT touched.
  OPTIM (optim.f + optim-tensor.f -> package OPTIM): names ALREADY clean, KEPT
    (SGD, SGD-MOM, WEIGHT-DECAY, ADAM/-M/-V/-W, TT-ADAM!). optim-tensor.f needed
    ONLY its package line (ADAM is same-package bare; T-GET/T-SET global bare).
    Callers: train.f (OPTIM:SGD), optim-test.f (7 words), optim-tensor-test.f
    (OPTIM:TT-ADAM!). FALSE-POSITIVES avoided: T-SGD! (array), MLP-SGD (mlp),
    G-SGD (gpu) all matched `\bSGD\b` but are DISTINCT words — not qualified.
    Also typed TRAIN-STEP's float locals (w/x/t/pred/dpred/dw :r) to clear the
    diff-lint on the touched line.

KEY STRUCTURAL FINDING (changes the migration model + is the namespace-lint's
main target): the "flat MAKI" is NOT uniform. MANY foundational maki files define
their words at TOP LEVEL / GLOBAL with NO package at all — array.f (T-AT/T-GET/
T-SET/T-SGD!/T-ADD!...), and nearly the whole eval cluster (eval.f, eval-compare
.f, eval-fixture.f, eval-repair-loop.f, eval-repair.f, eval-device.f,
eval-device-sm.f — only eval-author.f is `package MAKI`). Consequence: their
words resolve by GLOBAL fallback from inside any package, so they need NO
qualifier when called from a new subsystem package (this is why LOSS/OPTIM call
T-GET bare). The namespace lint must flag these GLOBAL maki defs FIRST — they are
literally "top-level maki definitions outside a package". Files still on real
`package MAKI` are the ones that DO force MAKI: qualifiers across a new seam.

REMAINING CLUSTERS — classified by blast radius + gating (recommended order):
  Tractable-but-medium (fully gated by maki/test.f, do next, one commit each):
    - TENSOR (tensor.f + tensor-value.f): tensor.f leaf, clean stems (DT-*/
      SHAPE-* are DOMAIN not package, KEEP); tensor-value.f TV-*/PLAN-*/PLINEAR.
      Inbound is wide because DT-F32 & TV-* are used across model-ir/move-facts/
      plan-ops/cad. Medium-high.
    - FUSION (fold fusion-plan.f into existing `package FUSION`): inbound wide
      (cad, lower-ew/mm/move/red/golden, sched-key, traffic all require it).
  Leaf, HIGH inbound (mechanical but large; short-name collision risk — do with
  perl-on-noncomment + gate, carefully):
    - ARRAY (array.f, currently GLOBAL): ~19 caller files; T-GET/T-SET/T-SGD!
      everywhere. Biggest single-leaf blast.
    - REPORT (report.f leaf, body unchanged): RPT-* in 22 files; the SHORT
      constants V-*/RC-*/CO-*/G-* leak into ~25 files INCLUDING ungated device
      tests (ablate-golden-device-test, lower-*-device-test, precision-device-
      test) — so maki/test.f alone does NOT prove it; needs the device gate too.
    - op-kind (OP-* FACTs): ~15+ callers. Foundational.
    - AUTOGRAD (autograd + autograd-tensor + adjoint + backward + gradcheck): 5
      files, adjoint/backward/gradcheck carry heavy downward deps.
  Root clusters, self-contained inbound (tests only) BUT partially ungated:
    - EVAL (flagship, GRADE-*): eval.f/eval-repair-loop.f core IS gated, but
      eval-author.f, eval-device.f, eval-device-sm.f, eval-compare.f are NOT in
      maki/test.f (eval-author-test/eval-device-test/eval-device-sm-test are
      separate, some device-gated/off-device-skip). A COMPLETE EVAL needs those
      run too, else it is a partial cluster (forbidden). Also naming collision to
      resolve: EV-* (eval core) and ER-* (eval-repair) BOTH want short tails
      (EV-RESET->? vs ER-RESET->?), and GRADE-* (author/device) -> EVAL:GRADE-*.
      Design the EV-/ER-/GRADE- tail scheme before touching it.
    - GPU (gpu.f/gpu-train.f, G-* stem): root, small, BUT its tests
      (gpu-test/gpu-train-test/gpu-sgd-test) are NOT in maki/test.f and are
      device-oriented; verify path needed before extraction.
  Heavy roots: TRAIN (train/mlp/linear/attention/embedding), CAD (cad/plan-*/
    model-ir core) — large, deep coupling; later.

MIGRATION-ORDER NOTE: the dot says "leaf-first". In a flat/GLOBAL namespace,
pure leaves (array, op-kind, report) have ZERO body change but the LARGEST
inbound blast per commit; roots have small inbound (tests) but rework their
downward MAKI: refs as lower layers later extract. The 3 landed clusters were
chosen as the low-blast intersection (few callers, distinctive names, fully
gated). Whichever direction the next session picks, the maki/test.f checker is
fail-closed on any missed qualifier (unresolved word -> red), so it is the
safety net — but device-only tests are OUTSIDE it, so leaves whose constants
leak into device tests (REPORT) or root device clusters (GPU, eval-device) MUST
add the device gate to their green proof.

DEFERRED (spec 2): maki.f top-level curated re-export is NOT started — it needs
the compiler EXPORT capability (dot habu-compiler-pkg-re-688212c1). No maki.f
exists yet; consumers still `require maki/<file>.f` directly and now call the
new subsystem packages pkg-qualified (ONNX:/LOSS:/OPTIM:). Keep those require
paths working until EXPORT lands; then maki.f becomes the require-aggregator +
curated EXPORT of the public interface.

STILL-TO-BUILD (dot scope): the namespace-lint tool (Habu-native, checked) that
flags top-level maki defs outside a package (whitelisting E-* cross-cutting
constants) and legacy BEGIN-/END- pairs. Primary targets it should catch today:
array.f + the GLOBAL eval cluster. Not built this session; needs its own commit
+ test.
