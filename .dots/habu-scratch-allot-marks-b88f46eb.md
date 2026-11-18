---
title: SCRATCH-ALLOT marks cells at an unaligned here
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T18:23:41.434115+01:00"
---

Problem: src/core/pointer-storage.f:6-9 SCRATCH-ALLOT marks a scratch cell every 8 bytes from `here` as found; after `1 c,` the marks are unaligned and the link rejects the build E-COMP-STATE (src/habu/linker.f:2129, test/fixtures/file-objects/unaligned.f), though its comment says it reserves at a cell-aligned `here`. Acceptance: SCRATCH-ALLOT aligns `here` first, so `1 c, 8 SCRATCH-ALLOT` builds and links. Depends: none. Claim: unassigned.
