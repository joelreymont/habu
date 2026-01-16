---
title: Fix emit error handling
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T12:08:23.158158+02:00"
---

src/bytecode/emit.zig:853 - Uses catch return error.OutOfMemory instead of try, rule violation. Use try. Low severity.
