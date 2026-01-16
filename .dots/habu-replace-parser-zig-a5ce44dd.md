---
title: Replace parser.zig symbol dispatch
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T16:21:19.267806+02:00"
---

src/reader/parser.zig:489,491,493,495,497: Replace std.mem.eql(u8, text/name, nil/t/and/or/not) with Value identity checks. <20min
