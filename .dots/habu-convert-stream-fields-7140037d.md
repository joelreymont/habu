---
title: Convert Stream fields to Values
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T16:16:13.946418+02:00"
---

Files: src/runtime/objects.zig, src/runtime/primitives/io.zig
From audit list, convert raw pointers to Values:
- file_path: raw pointer → String Value
- Other fields as identified
Update all Stream creation/access code.
Dependencies: habu-audit-stream-obj-c221359a
Verification: Stream struct updated, creation code compiles
