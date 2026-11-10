---
title: Move every scratch mark onto the declaring words
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T14:03:49.387310+02:00"
blocks:
  - habu-add-scratch-allot-07fcc86b
---

Problem: 57 lines call scratch-cell-mark and 23 call a private SCRATCH-BYTES loop after declaring storage (design .jj-ws.noindex/jerry-wasm/.out/scratch-declared-design.md, section 5, lists each site). Acceptance: each site declares its storage with SCRATCH-VARIABLE, SCRATCH-BUFFER: or SCRATCH-ALLOT; the three SCRATCH-BYTES loops (src/core/include.f:306, src/lib/build-out.f:23, src/lib/engine-id.f:24) and TASK-SCRATCH-CELLS, (src/lib/task.f:442) are deleted; the checker's }SCRATCH (src/core/checker.f:367) no longer marks and each variable inside a bracket is a SCRATCH-VARIABLE; scratch-cell-mark has no caller outside src/core/pointer-storage.f and src/habu/linker.f. Several briefs: library and tools; the checker's brackets; definers (layout-buffer.f DYNAMIC-BUFFER, task.f FACILITY, codegen.f CB-HEADER,, ffi-abi.f rows, intern-table.f). Verify: engine built twice equal; the suite at the landing tip. Depends: the words dot, built into bin/hb first. Ownership: the files of each brief. Claim: unassigned.
