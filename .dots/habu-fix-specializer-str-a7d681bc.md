---
title: Fix specializer string compare
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T12:07:43.855279+02:00"
---

src/compiler/compile.zig:6011 - std.mem.eql on 't' specializer, rule violation. Compare interned symbol identity. Medium severity.
