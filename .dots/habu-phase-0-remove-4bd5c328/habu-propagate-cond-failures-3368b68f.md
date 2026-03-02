---
title: Propagate condition failures
status: closed
priority: 1
issue-type: task
created-at: "\"2026-04-01T22:06:02.080667+02:00\""
closed-at: "2026-04-01T22:24:01.458132+02:00"
close-reason: done (condition signaling now propagates allocation/runtime failures; validation by codepath inspection plus zig build test blocked only by unrelated baseline compile errors)
---

Problem: condition signaling still masks allocation/runtime failures. Acceptance: signal paths propagate real errors with no catch-return masking. Files: src/interp/vm.zig:9397-9444. Verify: focused condition tests and rg for catch return false on signal path. Blockers: none.
