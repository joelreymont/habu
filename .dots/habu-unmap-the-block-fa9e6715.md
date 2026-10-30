---
title: Unmap the block a checker store leaves when it grows
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T02:40:28.534289+02:00"
---

Problem: src/core/checker.f ARENA-BYTES-GROW (:285) allocates the new block, copies the old bytes and returns the new base; nothing unmaps the mmap block the store moved out of, so every grow (callers :294, :314, :325: the schema node, root and family stores among them) leaks the old mapping for the life of the process (tim 2026-10-30, found while root-causing habu-keep-warm-and-ff3c297b). Acceptance: a grown store releases its old block at the grow (or at the rollback that drops the growth, whichever owns it), and a regression row shows the process mapping count or resident size does not climb across repeated grow-and-rollback cycles. Files: src/core/checker.f ARENA-BYTES-GROW and its callers. Verify: the regression row; test/checker rows that grow stores; full suite (shared checker). Depends: habu-check-a-def-3f58f036 (check before change decides who owns a grown block). Ownership: src/core/checker.f ARENA-BYTES-GROW. Claim: unassigned.
