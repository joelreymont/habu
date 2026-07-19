---
title: Package image builder
status: open
priority: 1
issue-type: task
created-at: "2026-07-19T21:28:00.930318+02:00"
---

src/os/image-bytes.f:10-221 exposes 64 package-less image-construction definitions, including 47 M-* and six MBUF-* words plus generic mutable cursors and patch helpers. The module is active in bootstrap/fixpoint executable-image construction; callers need a bounded image writer, not global access to its buffer, offsets, encoders, and patch state. Put it in package IMAGE with a minimal reset/here/length/put*/patch*/bytes API, keep storage/cursors/primitive encoders/private validation private, and update ELF and target builders directly without aliases. Preserve byte order, bounds/error behavior, offsets, patch semantics, every produced image byte, and zero-allocation fixed storage. Add old-global/private reject fixtures and public qualified positives, including canaries and injected bounds/patch failures proving no partial mutation. Measure dictionary-name bytes, JIT/DATA/CODELEN, image-builder storage, and build throughput before/after. Verify macOS/Linux image goldens, bootstrap/recovery/fixpoint/AOT/snapshot paths, package/host/filemap/dot lints, and full native gate. Parent: habu-pkg-native-build-f598557c; coordinate ELF caller renames with its leaf.
