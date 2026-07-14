---
title: Implement CAD-NUM scalar roles
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-13T14:14:22.884869+02:00\""
---

Full context: MODEL-CAD-V2-PLAN.md B5.1 specifies scalar-only nominal byte-len, item-count, cell-count, index, byte-off, cell-off, alignment, positive-divisor, alloc-byte-len, and alloc-cell-count plus numeric-result<a>. Fix: add lib/cad-num-types.f in package CAD-NUM with public checked validators around private audited TRUSTED mints; zero remains valid for ordinary extents/counts/offsets, positive roles reject zero, and alloc-cell-count also rejects counts above MAX-N / CELL-BYTES. Acceptance: exact positive/negative/zero/max boundary matrix; raw n and cross-role swaps reject statically; no public raw mint/projection; TRUSTED/refine/trust inventory rows and removal conditions; package remains unsealed and no production entry loads it. Files: lib/cad-num-types.f, lib/cad-num-types-test.f, TRUSTED.md, focused manifest/filemap entries only in final integration. Verify: exact source/test loads, typed-local, refine/trust/host/dot lints. Depends logically on TVK-RAW fix habu-nominal-storage-raw-a3430ef2.

Claim: agent=cadnum workspace=.jj-ws/fable-cadnum
