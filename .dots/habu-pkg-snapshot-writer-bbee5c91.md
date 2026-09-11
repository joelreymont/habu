---
title: Package snapshot writer
status: open
priority: 1
issue-type: task
created-at: "2026-07-19T21:29:07.211650+02:00"
---

src/habu/snap-lib.f:8-224 and :246-284 exposes 42 SNAP-/SNC-/SND-* definitions outside package scope. It is required by snap.f; the existing SNAP-CLOSE-SEAM package protects only one inner seam, leaving snapshot buffers, record writers, dependency/name tables, validation, patching, and scratch state globally reachable. Put the complete writer in package SNAPSHOT while retaining any narrower protected seam required internally; export only the write/go or exact minimal cross-file API, keep all storage/codecs/helpers private, and update snap.f/build callers directly without aliases. Preserve snapshot format/version, dictionary/package/protected-WID records, dependency order, warm restore behavior, bytes, hashes, and errors. Add old-global/private rejects, qualified public positives, corrupt/truncated/capacity cases, and cold-to-warm byte/behavior identity tests. Measure dictionary-name bytes, JIT/DATA/CODELEN, snapshot size, write/restore latency, and warm startup before/after. Verify snapshot/warm-snapshot/AOT/bootstrap/fixpoint/both-target gates, package/host/dot lints, and full native gate. Parent: habu-pkg-native-build-f598557c.
