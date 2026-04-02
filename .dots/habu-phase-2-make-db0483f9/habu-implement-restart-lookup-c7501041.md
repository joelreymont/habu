---
title: Implement restart lookup and invoke
status: closed
priority: 1
issue-type: task
created-at: "\"2026-04-01T22:06:02.188077+02:00\""
closed-at: "2026-04-02T16:32:42.824113+02:00"
close-reason: "done: restart-case/find-restart/invoke-restart now use first-class restart objects with exact identity; validation: zig build, zig build test baseline unchanged"
blocks:
  - habu-implement-handler-dispatch-82bcad71
---

Problem: advertised restart surfaces are not truthful. Acceptance: restart-case, find-restart, invoke-restart, and load-abort paths work generically. Files: src/runtime/primitives/condition.zig, src/interp/vm.zig. Verify: restart regressions covering lookup and invocation. Blockers: habu-implement-handler-dispatch-82bcad71.
