---
title: Accept ;package as package closer
status: open
priority: 2
issue-type: task
created-at: "2026-07-12T07:35:51.388333+02:00"
---

Problem: new package APIs required by Model CAD V2 cannot follow docs/forth.md because the native compiler only recognizes legacy end-package; current bin/hb rejects ;package with E-UNDEFINED. Fix: phase 1 of habu-rename-end-pkg-b681e666: add a distinct ;package keyword label/data/CF entry in src/habu/habu1.f and src/habu/habu2.f routed to C-END-PACKAGE; mirror it in bootstrap/cg/forth.fs; teach src/habu/verify-source.f, tools/public-signatures-core.f, tools/namespace-lint-core.f, and tools/reserved-name-lint-core.f the alias; retain end-package unchanged. Files: those production files plus test/gate-dictionary-lib.f, tools/check-all-errors-test.f, tools/public-signatures-test.f, tools/namespace-lint-test.f, tools/reserved-name-lint-test-lib.f, lib/source-test.f, tools/hb-build-test.f, test/gate-build-size.f. Acceptance: both spellings and mixed case close exactly one package; alias outside a package returns rc 75; public qualified lookup and post-close global scope are correct; verify-source/all-errors/signature/namespace/reserved/source/AOT paths agree; native and bootstrap tables remain mirrored; candidate size ratchet is measured. Verify: typed-local-diff-lint; focused tool tests; dictionary slice; native fixpoint install; maki/test.f; bootstrap check-only alias smoke; full bin/hb --load test/run.f. Depends: land and clean current R3 IR/tensor worker workspaces first; parent dot habu-rename-end-pkg-b681e666 phases 2-3 remain separate.
