---
title: Extract reusable test suite framework
status: open
priority: 2
issue-type: task
created-at: "2026-06-30T16:50:26.570990+02:00"
---

Refactor current test suite machinery out of test/gate-stdlib-lib.f into a generic checked Habu framework with vocabulary test suite/test group/test, suite setup and teardown hooks, sequential/parallel named groups, reusable by maki/odin. Habu-specific warm image/cache/HABU_UNDER_TEST/process setup belongs in Habu test adapter files, not framework core. Deprecate gate/row terminology in new output/API while preserving existing behavior.
