---
title: Reconcile enforcing maki namespace lint with fable's subsystem-package design
status: open
priority: 2
issue-type: task
created-at: "2026-07-10T02:31:00+02:00"
---

Merge finding (type-families into fable, 2026-07-10): TFAM's `tools/maki-ns-lint.f`
(enforcing "every maki top-level def in `package MAKI`, else a `\ maki-ns-lint:
boundary <PKG>` marker matching the file's single package token") is structurally
incompatible with fable's authoritative 168-file Model-CAD maki design, which fable
owns:
  - fable uses many subsystem packages (EVAL/GPU/CUDA/FUSION/LOSS/ONNX/OPTIM/PLAN/
    MAKI-GRADE), and several files declare BOTH `package MAKI` and a subsystem package
    in one file (report.f = MAKI then REPORT; tensor-value.f = MAKI then TENSOR).
    MNL-SCAN-FILE's whole-file marker model (marker must match the FIRST package token)
    cannot express "the MAKI section is checked, the subsystem section is a boundary".
  - fable intentionally keeps the documented ARRAY substrate (maki/array.f T-GET/T-SET)
    and leaf test scaffolding at GLOBAL scope so subsystem-package callers (e.g.
    `package GPU` gpu-train.f) resolve them by bare name; globals cannot carry a marker.

fable's own `tools/namespace-lint.f` already enforces the SAME maki-namespace-hygiene
goal compatibly and passes clean on the merged tree (0 global-def findings, 80 files);
it is in the required gate battery. maki-ns-lint therefore duplicates the goal with an
incompatible model — the same way TFAM's per-file `package MAKI` wrapping of individual
maki files was superseded by fable's subsystem-package refactor.

Resolution taken in the merge: `tools/maki-ns-lint{,-core,-test}.f` were UNGATED (removed
from gate-stdlib-cases/-lib/-lint-tools/-inline-lib and gate-stats stray-lint list) but
the tool files were KEPT (TFAM's work is not destroyed). namespace-lint remains the gated
maki-namespace guard.

Remaining work (pick one): (a) make namespace-lint enforcing (it passes clean, so this is
safe) and delete maki-ns-lint; or (b) rework maki-ns-lint to be package-SECTION aware
(check each `package .. end-package` region independently, exempt the documented ARRAY
substrate + test scaffolding like namespace-lint does) and re-gate it. Until then the maki
namespace is guarded report-only by namespace-lint.
