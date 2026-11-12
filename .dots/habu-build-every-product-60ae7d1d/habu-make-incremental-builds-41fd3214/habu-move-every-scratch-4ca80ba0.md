---
title: Move every scratch mark onto the declaring words
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T14:03:49.387310+02:00"
---

Problem: 38 lines outside src/core/pointer-storage.f and src/habu/linker.f still call scratch-cell-mark after declaring storage, among them src/core/include.f's private SCRATCH-BYTES loop (:291-302) and the checker's }SCRATCH (src/core/checker.f:1167-1172); design .jj-ws.noindex/jerry-wasm/.out/scratch-declared-design.md, section 5, lists each site. Acceptance: each site declares its storage with SCRATCH-VARIABLE, SCRATCH-BUFFER: or SCRATCH-ALLOT; include.f's SCRATCH-BYTES loop is deleted; }SCRATCH no longer marks and each variable inside a bracket is a SCRATCH-VARIABLE; scratch-cell-mark has no caller outside src/core/pointer-storage.f and src/habu/linker.f. Several briefs: library and tools; the checker's brackets; definers (layout-buffer.f DYNAMIC-BUFFER, task.f FACILITY, codegen.f CB-HEADER,, ffi-abi.f rows, intern-table.f). Depends: none. Claim: unassigned.
