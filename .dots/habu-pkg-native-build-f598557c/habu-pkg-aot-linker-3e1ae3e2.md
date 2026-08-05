---
title: Package AOT linker
status: open
priority: 1
issue-type: task
created-at: "2026-07-19T21:28:44.466750+02:00"
---

src/habu/aot-lib.f:21-361 exposes 74 package-less definitions: AOT-* plus generic linker/read/relocation/image helpers and mutable tables. It is loaded by non-REPL hb-build and consumed by aot.f, but consumers need one owned link pipeline rather than ambient access to record readers, relocation writers, buffers, and scratch state. Wrap the module in package AOT-LINK; export the minimal read/link/relocate/emit-seed operations actually crossing files, keep parsing, table, patch, validation, and output state private, and update aot.f/build callers directly with no aliases. Preserve every AOT record, relocation, owner/package entry, emitted seed byte, error, and deterministic ordering. Add old-global/private rejects, public qualified positives, corrupt/truncated/overflow negatives, and exact record/relocation/image goldens. Measure dictionary-name bytes, JIT/DATA/CODELEN, table storage, AOT output size, and link latency before/after. Verify AOT positive/negative/call/report, snapshot, bootstrap/fixpoint, both targets, package/host/dot lints, and full native gate. Parent: habu-pkg-native-build-f598557c; coordinate capture API with its separate leaf.
