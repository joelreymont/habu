---
title: Audit Stream object fields
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T16:16:07.527225+02:00"
---

Files: src/runtime/objects.zig
Review Stream struct for all fields:
- file_path: raw pointer? Should be String Value
- element_type: raw pointer? Should be Value or keep as metadata
- pushback_char: needs Value tracking?
- Other fields
List which need conversion to Values.
Verification: audit complete, conversion list created
