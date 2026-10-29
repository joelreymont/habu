---
title: RING-SETUP leaks the io_uring descriptor and its mappings when RING-MAPS throws
status: open
priority: 2
issue-type: task
created-at: "2026-10-29T23:34:31.927111+02:00"
---

Problem: src/lib/aio.f (Linux io_uring path) RING-SETUP creates the ring descriptor and maps its rings; when RING-MAPS throws, nothing closes the descriptor or unmaps what was already mapped because START has no catch around it. Found by reading in the stdio lane's re-review (tim, 2026-10-29); not reproduced, Linux only. Acceptance: a RING-MAPS failure leaves no descriptor and no mapping behind (close and unmap on the throw path, or map before the descriptor is published); a row in aio's Linux test that forces RING-MAPS to fail and counts descriptors and mappings, failing on the current code; proven on ssh spark. Files: src/lib/aio.f, its Linux test. Verify: the row on spark; tools/check.f on aio.f. Depends: none. Ownership: aio. Claim: unassigned.
