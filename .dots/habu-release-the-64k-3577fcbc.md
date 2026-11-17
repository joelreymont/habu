---
title: Release the 64K mapping in MEMT-TYPED-64K
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T10:56:42.272438+02:00"
---

Problem: src/lib/memory-test.f MEMT-TYPED-64K does MEM-64K MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop (src/lib/memory-test.f:214) and never releases the mapping. Acceptance: the case frees what it allocates. Files: src/lib/memory-test.f. Verify: memory-test green. Depends: none. Ownership: that case. Claim: unassigned.
