---
title: Tie copied error codes to their owners
status: open
priority: 3
issue-type: task
created-at: "2026-10-02T14:12:40.502097+02:00"
---

Problem (lane 321 r4-throwlint, 216a3929): src/core/generated-declaration.f:144-165 copies other files' throw codes as numbers under C- names, which tools/error-code-lint never sees, so a renumbered owner leaves them stale silently (lane 321 fixed C-PF-* by hand); tools/perf-map-test.f:108 '7402 die' uses a status above 255 that cannot be an exit code (exits 67) and has no E- name; lib/json-read.f:86,91 E-UPPER 69 and E-LOWER 101 are ASCII bytes named like error codes. Acceptance: the C- copies read their owners' constants by name (top-level alias reads, as 216a3929 does elsewhere; mind load order: the owner must be loaded first, or the copy moves to where it is); the perf-map die uses a real exit status or a named code; the json-read bytes get names that do not start with E-; tools/error-code-lint.f 0 findings; engine rebuilt if baked files change (g1 == g2, two-gen). Files: src/core/generated-declaration.f, tools/perf-map-test.f, lib/json-read.f.
