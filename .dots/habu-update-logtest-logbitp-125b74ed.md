---
title: Update logtest/logbitp VM opcodes
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T10:24:05.862959+02:00"
---

src/interp/vm.zig - Find logtest/logbitp opcode handlers:
1. Change 'const result = primitives.logtest(a, b)' to 'try'
2. Same for logbitp
Depends: habu-change-logtest-logbitp-232689a5
Verification: (logtest 'foo' 5) raises TypeMismatch, not false
