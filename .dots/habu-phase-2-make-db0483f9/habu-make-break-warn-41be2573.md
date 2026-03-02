---
title: Make break warn debugger flows truthful
status: open
priority: 2
issue-type: task
created-at: "2026-04-01T22:06:02.200137+02:00"
blocks:
  - habu-fix-nlx-control-dcb701b2
---

Problem: break, warn, debugger, and load-abort flows still misreport or short-circuit real condition state. Acceptance: debugger-facing flows follow actual condition and restart semantics with no fake success. Files: src/runtime/primitives/condition.zig, src/interp/vm.zig runtime entry points. Verify: focused break/warn/load-abort regressions. Blockers: habu-fix-nlx-control-dcb701b2.
