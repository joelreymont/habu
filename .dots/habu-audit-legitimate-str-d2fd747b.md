---
title: Audit legitimate str equality
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T16:21:20.180368+02:00"
---

Review remaining std.mem.eql(u8, ...) in: compile.zig (param/var name), emit.zig (block/tag), type.zig (field name), primitives/string.zig (actual string content). Add comments documenting why string comparison is correct. <30min
