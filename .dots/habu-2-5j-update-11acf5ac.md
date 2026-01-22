---
title: 2.5j Update VM string ops
status: open
priority: 2
issue-type: task
created-at: "2026-01-22T14:41:04.330109+02:00"
---

File: src/interp/vm.zig
string-length → codepoint count, char/schar → O(1) index.
