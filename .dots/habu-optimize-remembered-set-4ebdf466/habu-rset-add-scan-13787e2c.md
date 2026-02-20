---
title: "RSet: add scan fast paths"
status: open
priority: 1
issue-type: task
created-at: "2026-02-20T08:58:56.318873+01:00"
blocks:
  - habu-rset-tighten-card-ba8ce5c2
---

src/runtime/gc.zig: implement clean-run skipping and cache-friendly card scanning loops.
