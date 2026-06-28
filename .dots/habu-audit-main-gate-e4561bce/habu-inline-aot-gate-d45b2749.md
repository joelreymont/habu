---
title: Inline AOT gate assertions
status: open
priority: 2
issue-type: task
created-at: "2026-06-28T19:06:22.753803+02:00"
---

Problem: positive/negative AOT gates have in-process hb-build helpers, but test/gate-build-common.f still launches bin/hb for tools/aot-call-report.f and tools/gate-json-assert.f. That adds cold tool runs after each build even though gate-json-assert-core.f exists and aot-call-report can be factored similarly. Fix: expose checked library words for call-report extraction and JSON assertions, call them directly from gate-build-common/gate-aot-positive/gate-aot-negative, and keep one CLI wrapper test for each tool. Files: test/gate-build-common.f, test/gate-aot-positive.f, test/gate-aot-negative.f, tools/aot-call-report.f, tools/gate-json-assert-core.f. Acceptance: GB-AOT-REPORT/GB-GJA no longer spawn bin/hb for semantic assertions; CLI contract tests remain; AOT slices and full gate pass.
