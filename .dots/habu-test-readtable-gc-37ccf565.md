---
title: Test Readtable GC correctness
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T16:17:17.332517+02:00"
---

Files: tests/
Add tests:
- GC during read with custom reader macro
- Reader macro function callable after GC
- Dispatch macro (#+ #-) works after GC
- Readtable integrity after GC
Dependencies: habu-add-readtable-vm-f9870a27
Verification: zig build test passes, readtable GC correct
