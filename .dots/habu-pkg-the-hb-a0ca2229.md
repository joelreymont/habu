---
title: Package the hb-build library and its dependents
status: open
priority: 2
issue-type: task
created-at: "2026-08-11T01:12:00.000000+02:00"
---

Prerequisite discovered by the fixpkg lane (2026-08-11): tools/hb-build-lib.f is 206 unpackaged global definitions and tools/hb-build-test.f 145 more, so any signature change to their words (the content-key fold handle needs ~12: HBB-KEY-FILE+, HBB-CLOSURE-CK+, HBB-PRESEED-CK+, HBB-MAKER-KEY!, HBB-ARTIFACT-KEY!, ...) is rejected by package-diff-lint (probe: one-line body edit to HBB-KEY-FILE+ at tools/hb-build-lib.f:469 -> E-PACKAGE-OWNERSHIP, threw 1). Package hb-build-lib.f and hb-build-test.f; cascade reaches tools/hb-build.f, tools/hb-build-direct-lints.f, tools/hb-build-direct-lints-test.f, test/gate-aot-positive-lib.f, test/gate-build-hbb.f (~50 public words, measured by what callers actually use). Recipe proven in the BUILD-FIXPOINT packaging commit (merged 2026-08-11): one EXPORT block per surface, using-imports, NO renames; top-level drivers and any VERIFY:SOURCE-BUF/build call move after ;package; CHECKER-DEFINED? guards go public and are asked with the package closed. Gates: package-diff-lint 0, typed-local-diff-lint 0, fixpoint byte-identical x2, full battery. Files: the seven above. Depends: none. Blocks: habu-content-key-folds-9d2888c2.
