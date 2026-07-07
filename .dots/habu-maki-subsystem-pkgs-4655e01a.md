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
