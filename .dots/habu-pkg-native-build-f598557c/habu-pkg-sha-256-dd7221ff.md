---
title: Package SHA-256 support
status: open
priority: 1
issue-type: task
created-at: "2026-07-19T21:27:50.813797+02:00"
---

src/core/sha256.f:4-276 exposes 84 globals, including 54 SHA-* and six SHA256-* words, state arrays, schedule scratch, update/final helpers, file buffers, and hex rendering. It is loaded as a native prefix and used by content keys, engine identity, nominal codecs, signatures, and build tools, but consumers need only reset/update/final/digest/file/file-hex/hex APIs. Wrap the module in package SHA256, export that minimal checked surface, keep compression rounds, schedule/state cells, cursors, file scratch, constants, and casts private, and update all native-prefix and library/tool callers directly without forwarding globals. Preserve every standard vector, streaming/chunk behavior, file/error semantics, content key, engine id, snapshot/AOT identity, and exact digest/hex bytes. Add old-global and qualified-private reject fixtures plus public qualified positives. Measure dictionary-name bytes, loaded JIT/DATA/CODELEN, and digest throughput across small/large/chunked inputs; require no unexplained growth or regression. Verify SHA/content-key/engine-id/nominal/signature/build-cache tests, cold bootstrap, recovery, fixpoint, snapshot/AOT, package/host/dot lints, and full native gate. Parent: habu-pkg-native-build-f598557c; stage-0 mirror remains explicitly package-less.
