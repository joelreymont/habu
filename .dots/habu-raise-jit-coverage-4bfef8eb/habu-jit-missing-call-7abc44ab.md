---
title: JIT missing call targets
status: open
priority: 1
issue-type: task
created-at: "2026-02-17T22:23:16.858774+01:00"
---

src/jit/translate.zig and src/interp/repl.zig known-fn resolution. Cause: function designators/lambda call targets stay interpreted. Fix: generic call-target lowering and dispatch caching.
