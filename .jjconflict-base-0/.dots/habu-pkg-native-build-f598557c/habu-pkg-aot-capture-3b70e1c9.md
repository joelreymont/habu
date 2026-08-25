---
title: Package AOT capture
status: open
priority: 1
issue-type: task
created-at: "2026-07-19T21:28:57.582615+02:00"
---

Problem: `src/habu/aot-capture.f` owns record compaction, relocation sites,
name pooling, protected-WID capture, and the captured code blob, but its
representation casts, buffers, scanners, serializers, reset/report helpers,
and inline self-tests must not become public API.

Result: package the complete current module as `AOT-CAPTURE`. Export only
`CAPTURE` and `BOOTRUN+`; keep `AOT-DBASE`, `AOT-A>U8`, `AOT-N>U8`, record and
relocation helpers, name/protected-WID/blob state, serializers, and diagnostics
private. Coordinate `habu-move-aot-regressions-22c1ee71` so build-only inline
self-tests move to focused test source rather than remaining production
payload. Preserve compact-record inverse validation, relocation and name
tables, protected-WID validation, capture ordering, bootstrap behavior, and
exact captured blob bytes.

Acceptance: old global names and qualified private casts reject; both public
entries pass; corrupt records, relocations, names, protected-WID rows,
truncation, and capacity failures reject before publication; exact capture
tables and blob bytes remain unchanged. Measure dictionary-name bytes,
JIT/DATA/CODELEN, captured size, and capture latency. Run capture and AOT
positive/negative tests, bootstrap, fixpoint, snapshot, package and host gates,
then the full native gate. Parent: `habu-pkg-native-build-f598557c`.
