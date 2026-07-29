---
title: Reject nested package consistently
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T23:30:30.416731+02:00"
---

Full context: found by agent snapreloc 2026-07-30. A package NAME opened while another package is still open (no ;package between) COMPILES on the small engine load path but kills the snapshot build: the child dies with E-BUILD-STATUS (-2802) after printing a bare package. docs/forth.md says packages cannot nest, so the small engine accepting the nested open is the bug - same source, two engines, different acceptance. Make the small engine reject a nested package open at compile time with a named diagnostic (and a negative regression on the ordinary load path), so the failure is caught where the source is written instead of deep in the snapshot build. Reproducer: reopen any package inside another (the original shape was package SNAP-RELOC inside package SNAP in src/habu/snap-lib.f before commit 13bb611a moved the words). Checker-first note: the static invariant is that package scope is flat; the load path should enforce it, not the snapshot builder.
