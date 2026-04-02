---
title: Implement handler dispatch
status: closed
priority: 1
issue-type: task
created-at: "\"2026-04-01T22:06:02.182188+02:00\""
closed-at: "2026-04-02T11:14:22.674283+02:00"
close-reason: "done: handler-case/handler-bind now use runtime subtype dispatch; validation: zig build, zig build test baseline unchanged"
blocks:
  - habu-propagate-cond-failures-3368b68f
---

Problem: condition handlers are still stubbed or incomplete. Acceptance: handler-bind and signaling dispatch through real handler stacks. Files: src/runtime/primitives/condition.zig:65-166, src/interp/vm.zig condition paths. Verify: focused handler dispatch regressions. Blockers: habu-propagate-cond-failures-3368b68f.
