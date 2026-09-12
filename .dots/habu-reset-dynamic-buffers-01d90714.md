---
title: Reset dynamic buffers at capture so no image carries a dead mapping
status: open
priority: 2
issue-type: task
created-at: "2026-09-12T18:03:56.983081+03:00"
---

Problem: a DYNAMIC-BUFFER's control record (capacity and mapping pointer) lives in DATA; a build that reserved one bakes a non-zero capacity with a mapping address of the build process, DYNAMIC-STORAGE:RESERVE's early return on need <= old then trusts it in the product, and the generated reader's bounds check passes against the baked capacity: the combine lane's opcode map read through raw pointer arithmetic and SIGSEGVed generation 2 of the chain (2026-09-12); the existing module-sized scratch buffers escape only because a larger module forces a grow. Acceptance: at capture every dynamic buffer is released and its control record zeroed (a registry of dynamic buffers the definer keeps, or the capture lifecycle hook walking the declared records), or at boot the control records are reset before first use, stated in src/core/layout-buffer.f and docs/forth.md; a regression that reserves a buffer, captures an image, and reads through the buffer in the restored engine without a fresh reserve. Files: src/core/layout-buffer.f, src/core/dynamic-storage.f, src/habu/aot-capture.f or the lifecycle hook, test/. Verify: the regression, the five-generation chain. Depends: none. Ownership: hazel. Claim: unassigned.
