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

---

PROGRESS 2026-07-07 (session 2, workspace .jj-ws/fable-pkgs, host-side). On top
of the session-1 base (fable). maki/test.f green 73/73 after every commit;
test/run.f green (native suite ~6.8s < budget, run with fork enabled);
filemap-lint/host-lint 0 findings; typed-local-diff-lint clean. Commits:
  - eb889ef4  maki: tensor-value store+plan into package TENSOR
  - 9f872cec  tools: namespace-lint for global maki defs (ledger)

CLUSTER LANDED — TENSOR (substrate-split, NOT the naive whole-file package):
  Measured the real blast radius first. DT-F32 appears 157x and LAY-ROW 177x
  across maki — these dtype/layout/align ENUMS are pervasive VALUE-TYPE SUBSTRATE
  (like `true`/`false`), not a packageable subsystem; TENSOR:DT-F32 on every
  tensor literal is huge churn for NEGATIVE readability. So the split is:
    - value-type vocabulary (tensor.f DT-*/DT-VALID?/DT-SIZE/SHAPE-*/DIM-*/
      BCAST-SHAPE/TENSOR-BYTES + tensor-value.f LAY-*/AL-*/AL-VALID?) STAYS
      package MAKI substrate. Bare from every MAKI consumer via same-package
      resolution -> ~0 caller churn; ~520 refs untouched. In tensor-value.f these
      moved into a small leading `package MAKI ... end-package` block.
    - the STATEFUL tensor-value handle store + descriptor PLAN builder (TV-*,
      PLAN-*, PLINEAR, PGELU, TV-LINEAR) -> package TENSOR. This is a real module
      (mutable table + IR plan), the right thing to package.
  Inbound was BOUNDED to 8 files (cad, model-ir[comment-only], plan-ops, cad-test,
  cad-ref-test, plan-compose-test, plan-vocab-test, tensor-value-test) — the
  store/plan is used narrowly by the planning core while the enums are used
  everywhere, which VALIDATES the split. ~293 TV-/PLAN- refs qualified TENSOR:
  (exact-word, code-only via a scratch perl + the fail-closed maki/test.f as the
  oracle). CAUTION resolved: PLAN- is NOT a safe prefix — plan-ops.f (PLAN-UNARY/
  -LINEAR/-BIN-EW/...) and cad.f (PLAN-SHP-*/PLAN-REF) own other PLAN- families
  and plan-vocab.f already writes MAKI:PLAN-UNARY; only the exact tensor-value
  PLAN words were qualified. Downward from TENSOR: MAKI:DT-*/LAY-*/AL-*/OP-*
  (op-kind still MAKI) + MAKI:LINEAR. TV-CAP/PLAN-CAP promoted to public (a
  fixed-capacity store's capacity is legitimate contract; the E-TV-FULL /
  E-TV-PLAN-FULL fail-closed tests need them). Fully gated by maki/test.f (all 8
  consumers transitively loaded by gated suites; NO device-test uses TV-/DT-).

CLUSTER LANDED — namespace-lint (dot cluster 4; the ledger that drives the rest):
  tools/namespace-lint{-core,,-test}.f — checked Habu on the tools/lint framework
  (TOKENIZE/TOK/TN#, LINT-* helpers), sibling triad like maki-dep-lint. Tracks
  package/end-package depth; every definition at depth 0 (global, outside any
  package) in a maki/*.f is a finding. Whitelist: E-* error constants; the
  documented maki/array.f ARRAY substrate; BEGIN-/END- legacy pairs (separate
  tally); *-test.f scaffolding. String bodies skipped via quote-parity (so a
  defining word inside `s" ... "` never reads as a def — caught 3 false `:`/`"`
  during dev). REPORT-ONLY (never throws) while eval/gpu await packages;
  NAMESPACE-LINT-STRICT throws for future enforcement. Wired into the gate
  (test/gate-stdlib-cases.f suites), registered in FILEMAP.md.
  CURRENT LEDGER (verbatim count): 76 maki files, 126 global-def findings, 0
  legacy-pairs. By file: eval-device 32, eval-device-sm 27, gpu 21, eval-compare
  17, eval-repair-loop 11, eval 7, gpu-train 6, device-smoke 3, tensor-value 1
  (DEFTYPE tensor), report 1 (DEFTYPE report). => eval cluster 94, gpu cluster 27,
  device-smoke 3, two DEFTYPE value-handles 2. These GLOBAL clusters are the real
  remaining PACKAGE work; the lint is their TODO ledger.

SUBSTRATE POLICY (the load-bearing session-2 decision; generalizes the prompt's
ARRAY steer). Maki has a VALUE-TYPE / ENUM SUBSTRATE layer that must NOT be forced
into per-subsystem packages: forcing it is hundreds of qualified refs of churn for
worse readability, and global/same-package fallback keeps bare refs working, so
the "gain" is negative. Documented substrate exceptions (explicit, recorded here
+ the lint whitelist / package-MAKI membership):
  - ARRAY (array.f, GLOBAL): T-GET/T-SET/T-SGD!/T-ADD!/... — the value-array
    substrate, used bare across ~19 maki files AND lib/ptx tests. VERDICT: keep as
    GLOBAL substrate (lint whitelists array.f). Packaging = biggest single-leaf
    churn for arguable gain (the prompt's own read; confirmed).
  - op-kind (op-kind.f, package MAKI): OP-ADD..OP-N model op-kind enum, used bare
    in ~40 files (DIFFERENT enum than fusion.f's local OP-SCALE/ADD/RELU — do NOT
    conflate). VERDICT: value substrate; stays package MAKI, NOT its own package.
  - tensor value-type enums (see TENSOR cluster): DT-*/LAY-*/AL-*/SHAPE-*/DIM-*.
    Stay package MAKI substrate.
  - report enums V-*/RC-*/CO-*/G-*: verdict/roofline/coalescing/gate tags, ~26
    files incl device tests. Substrate; stay MAKI even when RPT-* packages.
  - E-* error constants: cross-cutting, top-level, whitelisted.
  Rule of thumb: a family that is a stateful MODULE (a store, a plan, a builder,
  a planner) -> package; a family that is primitive VALUE-TYPE VOCABULARY (dtype/
  layout/align/op-kind/verdict enums + pure predicates) -> shared substrate.

REMAINING CLUSTERS — reranked with session-2 blast-radius data:
  - REPORT: RPT-* report-builder API in 22 files (only 1 device test:
    lower-model-device-test) is a genuine subsystem -> package REPORT via the SAME
    substrate-split (RPT-* packaged; V-/RC-/CO-/G- enums stay MAKI substrate,
    bare, so the device-const leak the s1 note feared is a NON-issue for the enum
    half). report.f already has E-RPT-* top level + a `package MAKI` body; DEFTYPE
    report is the 1 lint finding there. RE-MEASURED, heavier than it looks: the
    substrate-split costs ~66 internal V-/RC-/CO-/G- refs inside report.f's render
    machinery (R-*/H-/P-/EMIT-* + RPT-* validators) that need MAKI: quals; the
    whole-file alternative avoids those but pushes V-/RC-/CO-/G- into 26 inbound
    files incl 7 device tests (pending-zed). Do the substrate-split, but budget
    the 66 internal quals + 22-file RPT- inbound as a full cluster of its own.
  - FUSION (fold fusion-plan.f -> package FUSION): HEAVY ROOT, re-measured. FP-*
    inbound is 33 files incl ~13 UNGATED device tests (lower-*-device-test), and
    fusion-plan.f needs ~60 downward MAKI: quals (CLASS-* 25, MIR-* 25, OPR-CLASS,
    MV-*, RPT-SPLIT+) that are transitional (redone when model-ir/op-registry/
    move-facts extract). s1 called it "medium"; the body says otherwise. DEFER:
    high churn + device-entangled + not gate-provable host-side. Do after model-ir/
    op-registry are packaged (so the downward quals land final), with the device
    gate (pending-zed).
  - AUTOGRAD (autograd/autograd-tensor/adjoint/backward/gradcheck, all MAKI):
    names are clean domain scalar VJPs (ADD-F/ADD-BWD/MUL-BWD/RELU-F...) — GENERIC
    and collision-prone; inbound not yet safely characterized. Needs a careful
    per-word assessment (which -F/-BWD are the public VJP table vs helpers) before
    a package name/qualify pass. Deferred pending that assessment.
  - EVAL / GPU (the GLOBAL clusters, = the 126 lint findings): the real remaining
    PACKAGE work. Large + device-entangled + need the EV-/ER-/GRADE- tail scheme
    (s1 note). Left RED in the report-only lint as the driving TODO ledger.

DEFERRED (spec 2), unchanged: maki.f curated re-export still needs compiler EXPORT
(dot habu-compiler-pkg-re-688212c1). Consumers still require maki/<file>.f and
call ONNX:/LOSS:/OPTIM:/TENSOR: pkg-qualified.

---

PROGRESS 2026-07-08 (session 3, workspace .jj-ws/fable-pkgs, host-side). On top
of the session-2 base (fable = 6c601eb9). maki/test.f green 73/73 after every
commit; test/run.f green at checkpoint + final (6.8s < budget); typed-local-diff-
lint clean per commit and final. Commits:
  - e9e1202b  maki: report builder into package REPORT
  - 8e9f3b4b  maki: eval core into package EVAL
  - 5898c050  maki: eval repair-loop into EVAL REPAIR- module
  - e641a455  maki: tensor DEFTYPE into package TENSOR
  - 3197548c  STATUS: roll last-verified to 2026-07-08 (daily stale-status lint)
LINT LEDGER (verbatim tail counts): 126 -> 125 (REPORT) -> 118 (EVAL core)
  -> 107 (REPAIR-) -> 106 (tensor DEFTYPE). Remaining 106 by file: eval-device 32,
  eval-device-sm 27, gpu 21, eval-compare 17, gpu-train 6, device-smoke 3.
  ALL remaining findings are device-entangled (pending-zed).

CLUSTER LANDED - REPORT (substrate-split as prescribed):
  report.f -> enum block V-*/RC-*/CO-*/G-* stays a leading `package MAKI public`
  block (substrate; device tests + whole CAD pipeline read them bare); the
  stateful builder/renderer -> package REPORT. Public names STRIP the RPT- stem
  (REPORT:NEW / MODEL! / GATE! / RENDER / WARN+ / HOT+ / GATE-TAG@ ...) because
  RPT *is* the package name (forth.md: don't repeat the package in the tail) -
  unlike TENSOR's TV-/PLAN- which name two distinct subfamilies. 51 public words;
  RPT-DROP + cap constants + arena/out buffers stay private with RPT- names.
  Private raw readers MODEL$/...(no-handle) renamed K-MODEL$/... to avoid
  private/public tail shadowing with the stripped public accessors. DEFTYPE
  report moved INSIDE package REPORT (casts >report/report>N are package-scoped;
  >report used only by NEW, report>N unused anywhere - verified; the checker
  registers the nominal TYPE globally so bare `report` in external signatures
  keeps working - fixture-proven). ~43 internal MAKI: enum quals landed (est. 66
  was high; def lines moved with the enums). Inbound: 23 caller files (all
  `package MAKI`) exact-word qualified REPORT:; comments/docs updated
  (docs/ablation.md, mem-plan/cad-test/traffic prose).
  TRAP CONFIRMED + AVOIDED: tools/repair-packet-test.f owns an UNRELATED RPT-*
  family (RPT-OUT/RPT-ERR/RPT-LABEL!...) and does NOT require maki/report.f -
  exact-word set only, never a prefix sweep (the s2 PLAN- lesson again).
  lower-model-device-test.f updated (2 sites), host-parse green (device leg
  SKIPPED off-device); device re-verify pending-zed.

AUTOGRAD ASSESSMENT (the per-word public-vs-helper verdict; no code change):
  Evidence gathered:
  - op-registry.f R-REF reference-binding table (the COMPLETE-membership gate)
    tick-binds ' ADD-F ' MUL-F(x2) ' RELU-F from autograd.f UNIFORMLY NEXT TO
    ' GELU-F (gelu.f), ' SILU-F (silu.f), ' LN-FWD (layernorm.f), ' RMS-FWD
    (rmsnorm.f), ' SM-FWD (softmax.f), ' ROPE-PAIR (rope.f), ' MOVE-* (move.f),
    ' MATMUL/' LINEAR. The scalar-reference vocabulary is CROSS-FILE; autograd.f
    owns only its primitive-arithmetic subset.
  - ONNX:LOWER maps ONNX ops to reference names AS STRINGS: s" ADD-F" s" MUL-F"
    s" RELU-F" - a string-level contract on the bare spellings.
  - executor.f dispatches ADD-F(x3)/MUL-F(x2)/RELU-F/RELU-BWD; softmax.f calls
    MAX-F; train.f calls MUL-F; autograd-tensor.f lifts ADD/MUL/SUB/SQUARE/RELU
    F+BWD. ALL 18 scalar words are referenced contract members (the vjp primitive
    table, dot habu-ad-vjp-primitive); NONE are internal helpers. No collisions:
    no other definition of any *-F/*-BWD name repo-wide.
  VERDICT (documented substrate, recorded here per the substrate policy):
  - autograd.f + autograd-tensor.f STAY package MAKI substrate. They are pure
    functions (zero module state) = the function-value side of the op-kind enum
    vocabulary; packaging autograd.f's subset alone would fracture ONE uniform
    reference vocabulary across two namespaces (registry ticks and ONNX strings
    would mix AUTOGRAD:ADD-F with bare GELU-F). Names already stem-free domain
    spellings; zero lint findings (already in package MAKI).
  - adjoint.f / backward.f / gradcheck.f ARE genuine stateful modules (fact
    registry + IR transform tables + gradcheck arena/gate wiring) and DO deserve
    package AUTOGRAD - but DEFERRED on measured grounds, same profile that
    deferred FUSION in s2: downward refs adjoint 57 OP + 2 OPR; backward 107 MIR
    + 15 MV + 25 OP; gradcheck 18 MIR + 10 EX + 3 OPR + 1 OP. Of ~238 downward
    quals ~155 (MIR/MV/OPR/EX) are TRANSITIONAL against unpackaged model-ir/
    move-facts/op-registry/executor (2.5x the FUSION deferral). Package AUTOGRAD
    after MIR/OPREG/EXEC extract. External inbound today is modest and all
    package MAKI (saved.f, from-scratch-train.f, cad.f + 4 tests) - nothing
    breaks by waiting. Side evidence for the generic-prefix risk: tools/ptx/
    bandwidth-lib.f owns a DISTINCT BW-* (bandwidth) family; packages fix this
    class of ambiguity.

EVAL TAIL SCHEME (designed; collisions pre-solved; pattern-setter + 1 landed):
  ONE package EVAL for the whole eval subsystem, reopened per file (multi-file
  package per forth.md). Rules:
  1. eval.f core = the package ROOT: EV- stem drops entirely. Landed:
     EVAL:CHECK-PASSES? (TRUSTED: boundary, name kept - descriptive not stem),
     EVAL:RESET / SCORE / PASS@1? and public tally variables EVAL:PASS /
     EVAL:TOTAL (direct `@` reads are existing contract). EV-RECORD had no
     external callers -> private RECORD. RESET-as-package-word follows the
     FUSION precedent (fusion.f RESET).
  2. Sub-modules keep a DOMAIN subfamily tail (TENSOR TV-/PLAN- precedent),
     which resolves the EV-/ER- tail collision (both wanted RESET/STEP/...):
     eval-repair-loop -> REPAIR- (LANDED: EVAL:REPAIR-RESET/-STEP/-ROUNDS@/
     -TOKENS@/-GREEN?; COUNT-TOKS + ER-* state private), eval-compare -> CMP-
     kept, device grading -> GRADE- kept (EVAL:GRADE-CANDIDATE reads right),
     device tally EVD-* -> DEVICE-, device-sm -> GRADE-SM-/SM- (s4 refines).
  3. PRIVATE names KEEP their per-module prefixes (ER-, CMP-, ED-, SM-):
     reopening a package RESUMES one shared private wordlist (no-duplicate set),
     so generic private names would collide ACROSS the package's files.
  4. Eval-internal verdict vocab (EVN-*) stays inside EVAL (unlike V-/RC-:
     nothing outside eval reads it) - final call in s4 with the device cluster.
  GATING: eval core + repair fully proven by maki/test.f (eval-test, eval-
  fixture, eval-repair, eval-repair-ab-test, cad-test, plan-vocab-test all
  gated). The touched 1-site CHECK-PASSES? callers eval-device.f/eval-device-
  sm.f host-load green through the maki/README.md Orin prelude (checker verifies
  the qualified refs at load); device re-verify pending-zed.

ALSO LANDED: tensor-value.f DEFTYPE tensor moved into package TENSOR public
  (casts are package-scoped - fixture-proven; tensor>N is externally used by
  cad.f + 3 tests -> now TENSOR:tensor>N, 17 sites; >tensor internal-only).
  Public because handle inspection is the audited representation boundary the
  tests legitimately consume. Nominal type `tensor` stays globally visible in
  signatures (checker registers types globally regardless of package).

REMAINING (all device-entangled, pending-zed; the 106-finding ledger):
  - eval-device.f (32) + eval-device-sm.f (27) + eval-compare.f (17, "load
    after eval-device.f", consumes GRADE-CANDIDATE/EVN-*): the EVAL device
    modules under the scheme above; need the Orin leg to re-verify.
  - gpu.f (21) + gpu-train.f (6): package GPU cluster, parked per the dot.
  - device-smoke.f (3): tiny; decide package vs test-scaffolding whitelist in s4.
  - maki.f curated re-export still blocked on compiler EXPORT
    (habu-compiler-pkg-re-688212c1).
