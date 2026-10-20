---
title: Read native FILE status spacing
status: active
priority: 2
issue-type: task
created-at: "2026-10-15T07:53:30.457110+03:00"
---

Actual native-build-edit E2E baseline now builds a runnable engine from628 source, but FILE-STATUS? constructs symbol.fkey= while native report is symbol.f key=. Habu s\" key=\" consumes its delimiter space. Fix the existing parser literal to retain the report separator; reuse the already-built baseline/log for this parser and runtime-probe check, then continue warm test without unnecessary cold rebuilds. Keep assertions on actual file hits and executable origin behavior; no looser substring or weakened expectations. Carl engine_shared_core_max owns fixture correction under existing native incremental59200861 acceptance; source-only fixture fix needs proportionate verification.
