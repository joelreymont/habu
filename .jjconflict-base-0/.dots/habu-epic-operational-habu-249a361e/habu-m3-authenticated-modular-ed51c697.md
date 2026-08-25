---
title: "M3: authenticated modular builds"
status: open
priority: 1
issue-type: task
created-at: "2026-07-17T13:03:36.609029+02:00"
blocks:
  - habu-m2-safe-filesystem-6d289b3d
---

Start only after M2 is closed on green master. Canonical cache-root resolution is already landed; own authenticated frozen source frames, exact include/require/provided execution semantics, initial registry identity, no reopen after freeze, exact diagnostics, cache/lint certificate identity, modular hb-build/AOT behavior, and removal of flat source composition. Existing owners: habu-build-exact-modular-44f4c2dc, habu-compile-authenticated-src-05e058a2, and habu-remove-synthetic-compose-373b117a. Finish with direct-versus-built parity, cold/hit/miss truth, native/recovery/AOT/fixpoint/full gates, then green master promotion.

RECOVERY POINTER 2026-07-18 (workspace forensic sweep): the exact-modular source composer for this milestone exists only in workspace habu-build-exact-modular-44f4c2dc at effe9d27, never on master: tools/source-compose.f, tools/source-map.f, tools/diag-remap.f, docs/modular-builds.md. This is the M3 blocker for the M4/M5 diff work. Recover from that tip; do not delete the workspace before then.
