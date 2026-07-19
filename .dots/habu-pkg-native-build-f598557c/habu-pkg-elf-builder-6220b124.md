---
title: Package ELF builder
status: open
priority: 1
issue-type: task
created-at: "2026-07-19T21:28:09.878565+02:00"
---

src/os/linux/elf.f:5-251 exposes 89 global definitions, including 51 ELF-* names plus header/segment/tail layout constants, offsets, cursors, and patch helpers. It is the active Linux bootstrap/fixpoint target, not a reusable public vocabulary. Wrap it in package ELF, export only the complete build and deliberately consumed tail/layout queries, keep header/segment field writers and mutable construction state private, and update target/build callers directly after habu-pkg-image-builder-6d75bf67. Do not package the stage-0 bootstrap mirror. Preserve exact ELF bytes, program/section headers, permissions, alignment, entry point, relocations/tail data, executable behavior, and failure bounds. Add old-global/private rejection fixtures and public build positives; parse the resulting binary independently to assert every header field and segment boundary. Measure dictionary-name bytes, JIT/DATA/CODELEN, output size, and build latency before/after. Verify Linux image/build/fixpoint/AOT/snapshot/launch gates, cross-target source closure, package/host/filemap/dot lints, and full native gate. Parent: habu-pkg-native-build-f598557c.
