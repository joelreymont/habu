---
title: JIT missing call targets
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-02-17T22:23:16.858774+01:00\\\"\""
closed-at: "2026-02-22T08:25:00.410492+01:00"
close-reason: "completed: rooted global_ref generic call-target lowering + regressions"
---

src/jit/translate.zig and src/interp/repl.zig known-fn resolution. Cause: function designators/lambda call targets stay interpreted. Fix: generic call-target lowering and dispatch caching.
