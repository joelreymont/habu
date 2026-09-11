---
title: Replace tests and tools tied to retired native paths
status: active
priority: 1
issue-type: task
created-at: "\"2026-09-11T09:05:33.554037+03:00\""
---

Owner: Cedar; test helpers and affected tools, excluding agent-owned checker/compiler storage. Replace observed bin/hb-host and retired IR/build assumptions with the existing bin/hb native load, published code and APP-IMAGE path; delete obsolete wrappers instead of restoring a second engine. Keep real semantic and rejection coverage. Acceptance: affected tests execute through the current native runtime and verify their original behavior; final suite acceptance remains in the existing integration child. Bootstrap pipeline replacement belongs to the native-build child.
