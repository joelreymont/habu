---
title: Implement handler-bind VM execution
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:37:51.717615+02:00"
---

src/interp/vm.zig: Add handler_bind case in execute() switch. Push handler onto dynamic handler stack, execute body, pop handler. Pattern after restart-case VM implementation. Dependencies: habu-add-handler-bind-bd72b724. Verify: run test case.
