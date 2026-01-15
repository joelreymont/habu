---
title: Design Stream object layout
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:38:47.349178+02:00"
---

src/runtime/objects.zig: Add Stream struct after line ~400. Fields: type (file/string/byte), buffer, position, open flag. Add Stream type tag to Value tagging scheme. Dependencies: none. Verify: struct compiles.
