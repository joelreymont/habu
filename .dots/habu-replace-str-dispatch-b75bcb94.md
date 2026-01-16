---
title: Replace string dispatch with identity
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T10:24:24.639657+02:00"
---

src/compiler/compile.zig:5229,5263,5532,5542 - Replace std.mem.eql comparisons:
1. Find all 'std.mem.eql(u8, key_str, ":")'
2. Replace with 'key.eq(self.builtins.kw_colon)'
3. Same for 'type' and 'initform'
Depends: habu-pre-intern-defclass-c1749374
Verification: defclass still works, tests pass
