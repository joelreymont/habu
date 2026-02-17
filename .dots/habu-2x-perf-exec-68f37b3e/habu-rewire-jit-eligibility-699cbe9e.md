---
title: Rewire JIT eligibility after API sync
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-02-17T11:21:56.812823+01:00\\\"\""
closed-at: "2026-02-17T11:58:12.348007+01:00"
close-reason: completed
blocks:
  - habu-adapt-habu-to-7e7240c7
---

After hoist API adaptation, re-validate and update JIT translation/eligibility constraints. Files: src/interp/repl.zig:2081-2138, src/jit/backend.zig translator gates and call lowering. Ensure new API semantics are reflected before coverage expansion.
