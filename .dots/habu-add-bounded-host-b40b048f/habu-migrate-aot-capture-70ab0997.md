---
title: Migrate AOT capture buffer to MEM spans
status: open
priority: 2
issue-type: task
created-at: "2026-07-13T16:45:06.197175+02:00"
blocks:
  - habu-freeze-transient-mem-1a69322a
---

Problem: one AOT capture buffer still relies on raw pointer/length state, so bounds, aliasing, mutation generation, and persistence are not statically tied. Fix: migrate the smallest complete AOT capture path to MEM owner/span/index/borrow/FREEZE APIs without adding trust; keep generated output byte-identical. Acceptance: raw pointer path is removed, stale or cross-buffer evidence rejects, capture output and cache identity remain byte-stable, cleanup consumes the owner on every error path. Files: one AOT capture owner file plus focused test and TRUSTED.md only if a row is discharged. Verify: exact AOT capture tests, AOT positive/negative gates, trust/typed-local lints, full native gate.
