---
title: Drop schema and format versions
status: open
priority: 2
issue-type: task
created-at: "2026-10-27T13:24:03.198957+02:00"
---

Problem: persisted formats carry version constants (schema_version appears nowhere in src, tools or test); Joel 2026-10-27: 'we have no legacy. hard cutover only!'. Acceptance: no schema_version field emitted or asserted; version constants that only gate old data deleted; a constant that keys a cache is replaced only where the key already covers the writer. Files: as listed plus rg -n 'schema_version|VERSION' census. Verify: rows of every touched tool. Depends: none. Ownership: carl. Claim: unassigned.

Also (2026-11-03): tools/hb-build-report.f writes `"version":1` beside `"schema"` in the report and the error object (REPORT$ :120, PATH-ERROR$ :141) and `version=1` in the error line (:147).
