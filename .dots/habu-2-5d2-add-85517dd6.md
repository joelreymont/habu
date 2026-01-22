---
title: 2.5d2 Add heap.allocString32FromUtf8
status: open
priority: 2
issue-type: task
created-at: "2026-01-22T14:40:39.703050+02:00"
---

File: src/runtime/heap.zig
Decode UTF-8 to codepoints, replace invalid with FFFD.
