---
title: Fix nested eval exits
status: closed
priority: 1
issue-type: task
created-at: "\"2026-02-17T22:23:16.853763+01:00\""
closed-at: "2026-02-19T23:45:41.671396+01:00"
close-reason: "covered by nested load/handler non-local-exit regressions in src/interp/repl.zig:4721 and VM unwind fixes"
blocks:
  - habu-rca-indirect-call-d9f594ad
---

src/interp/vm.zig non-local exits. Cause: nested eval and handler flows can lose cleanup/stack shape. Fix: consistent unwind and frame restoration rules.
