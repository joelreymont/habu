---
title: Widen the arm-if scratch pool or state its bound
status: open
priority: 2
issue-type: task
created-at: "2026-08-14T00:49:31.009976+02:00"
---

Found by the width-export landing: a widest payload crossing a branch inside a MATCH arm exceeds the suite's eight scratch registers (E-A64RA-SPILL under REGS 8; compiles under 18) - the E-ARMIF fixture states its own register count rather than moving the pinned cost measurements. Real measured limit of that pool; decide whether the pool widens or the bound is documented as the arm's contract. Files: src/compiler/native (allocator scratch pool). Depends: none.
