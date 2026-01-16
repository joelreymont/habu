---
title: Replace bicheck.zig type name dispatch
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T16:21:19.876110+02:00"
---

src/types/bicheck.zig:730,732,734: Replace std.mem.eql(u8, v.name, fixnum/string/symbol) with table-driven lookup over BuiltinSymbols type names. <25min
