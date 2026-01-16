---
title: Add Stream GC visitor
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T16:16:19.481842+02:00"
---

Files: src/runtime/gc.zig
Add visitStream() function:
- Rewrite file_path Value (if moved)
- Rewrite other Value fields
- Keep OS handles as integers (safe)
Add stream case to copyObject().
Dependencies: habu-convert-stream-fields-7140037d
Verification: streams traced during GC, Values rewritten
