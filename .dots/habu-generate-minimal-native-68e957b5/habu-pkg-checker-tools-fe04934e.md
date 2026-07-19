---
title: Package checker tools
status: open
priority: 1
issue-type: task
created-at: "2026-07-19T21:30:10.947678+02:00"
---

tools/check-core.f:22-1266 exposes 280 CHK-* or CHECK-MAIN globals; tools/public-signatures-core.f:9-899 exposes 222 PS-* globals. They are loaded by the checker CLI, diagnostics gate, and diagnostic worker; tests currently reach internal state directly. Sixty-two names exceed the inline dictionary limit, and prefix spelling provides no privacy for configuration, buffers, parser/replay state, signature tables, or lifecycle helpers. Put the modules in packages CHECK-CLI and PUBLIC-SIGNATURES. Export MAIN plus only the deliberate in-process capture/materialize/direct-run contract; tests reopen owner packages for white-box cases; update CLI/gate/worker callers directly and delete all CHK-/PS-/CHECK-MAIN aliases. Coordinate grammar behavior work in habu-tools-check-unified-fb3b67f6 and habu-tools-reflect-all-80b1aa58 without absorbing it. Preserve CLI text/JSON, source-list replay, all-errors behavior, direct-run captures, diagnostics, public-signature manifests/order/hashes, and exit codes exactly. Add old-global/private rejection and public qualified positives. Measure long-name/dictionary/JIT/DATA/CODELEN, startup/capture/replay throughput before/after. Verify checker/public-signature/diagnostic/worker suites, package/host/filemap/dot lints, fixpoint, and full native gate. Ownership: module boundaries and caller renames only.
