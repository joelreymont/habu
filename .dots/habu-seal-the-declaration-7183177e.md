---
title: Seal the declaration callback table
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T10:36:19.555621+02:00"
---

CG-22. Declaration callback-table growth uses anonymous mmap storage (src/core/checker.f:11-17,58-64); callback cells are persisted through xt! and snapshot metadata stores cell-DATA without proving the cell lies in aligned DATA. A capacity-1 coordinator grown by a second registration records an XTCELL offset outside [0, DATA-SIZE). Boot survives only because exactly five participants fit five rows. Fix: this is a sealed closed-world participant table — delete dynamic growth and allocator state, change the static capacity explicitly when membership changes, and make xt!, writer, and loader reject non-aligned cells outside DATA before mutation.
