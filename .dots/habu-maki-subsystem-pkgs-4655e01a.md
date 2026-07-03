---
title: "Maki: subsystem packages + maki.f re-export"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T01:10:04.572860+02:00"
---

Replace stem-prefix namespacing (gpu-*, eval-*, loss-*, GRADE-*, file-name stems) with real packages per docs/forth.md:70-89. One package per subsystem inside maki: TENSOR, ARRAY, LOSS, OPTIM, AUTOGRAD, TRAIN, GPU, EVAL, ONNX, FUSION (+ CAD/REPORT from cad-0a/0b when they land). Word names lose their stems (LOSS:NLL not GAUSSIAN-NLL-LOSS); file names lose stems where the package makes them redundant. Top-level maki.f re-exports the curated public interface via the re-export capability (depends: compiler re-export dot) so users load maki.f and call MAKI:* or subsystem packages directly. Migrate leaf-first, module-by-module, callers updated in the same increment, maki gate (bin/hb --load maki/test.f) green each step. Supersedes habu-roll-out-the-3bd15be6 (single flat MK: namespace) - close or fold that dot into this one. E-* error constants stay cross-cutting. Add the namespace lint idea from the old dot: flag top-level maki definitions outside a package.
