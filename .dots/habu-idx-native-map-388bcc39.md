---
title: Index native map aliases
status: active
priority: 2
issue-type: task
created-at: "2026-10-16T08:46:23.442552+03:00"
---

NAMES-ALIAS? scans all earlier captured records for every map row, making distinct spans quadratic on both cold and warm native builds. Build one row-indexed alias table per BUILD-NAMES by sorting eligible nonnamespace, nonnegative rows by (start, len, capture row); only later equal pairs are aliases. Preserve exact map bytes, including namespace and negative-start classification. File: tools/native-build-core.f. Existing native-host-bundle map/source-origin E2E and same-source map comparison; report actual map-phase CPU if stable timing exists. Integrator owns rebuilt engine/full gate, independent review and landing.
