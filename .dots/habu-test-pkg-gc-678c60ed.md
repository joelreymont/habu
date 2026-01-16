---
title: Test Package GC correctness
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T16:16:57.276999+02:00"
---

Files: tests/
Add tests:
- GC during package operations (intern symbol, GC, lookup)
- Symbol lookup after GC (verify not stale)
- Hash table integrity (rehash verification)
- Package with circular use lists
Dependencies: habu-add-pkg-vm-1df1e17a
Verification: zig build test passes, package GC correct
