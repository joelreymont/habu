---
title: Package vector module surface
status: active
priority: 2
issue-type: task
created-at: "2026-07-26T23:22:48.835637+02:00"
blocks:
  - habu-migrate-raw-vector-259d513e
  - habu-retire-raw-vector-14bb24b6
---

COORDINATION PARENT (redesigned 2026-07-26 late after the checkpoint falsified the original contract). The wall probe stands: any body edit in lib/vector.f's unpackaged region trips E-PACKAGE-OWNERSHIP (measured, whitespace-only edit to VEC-DISPOSE, lib/vector.f:178). But the original rename-map plan is IMPOSSIBLE, proven on the real file: package VEC's existing typed public API already owns the naturalized tails (INIT, CLEAR, DISPOSE, LEN@, CAP@, RESIZE, ENSURE, PUSH, EACH, @, !) - eleven collisions, duplicate-definition rc 78, eight with external callers, so the legacy words can be neither private nor public under their natural tails. The original cascade figure (353 references, 15 files) was sweep contamination: a -w sweep counts E-VEC-BOUNDS as a VEC-BOUNDS caller; the exact-token truth is 210 external references in 6 files, and the originally named acceptance suites (schedule, model-ir) have ZERO legacy callers. RULING: no RAW-* bridge surface - it would permanently publish words lib/vector.f's own comment schedules for retirement. The correct fix is the retirement itself: leaf habu-migrate-raw-vector-259d513e moves the five real caller files onto the existing typed API (semantic role conversion, not rename), then leaf habu-retire-raw-vector-14bb24b6 deletes the raw surface and packages everything that survives. The held MEM:RELEASE rename artifact unblocks when leaf 2 lands. LESSON recorded: a measured number in a dot contract must name its sweep method; -w over hyphenated Forth names is contamination by construction.
