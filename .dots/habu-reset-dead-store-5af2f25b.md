---
title: "Reset dead store tails at capture so the chain's byte fixpoint moves to generation 2"
status: open
priority: 2
issue-type: task
created-at: "2026-09-12T13:04:35.356664+03:00"
---

Problem: an engine image bakes each persisted store's whole boot buffer (SYM-STR-BOOT is 1 MB of DATA whose live content ends at 182754 bytes) and the capture writes DATA as its non-zero extents, so build-time residue in dead tails (21 one-byte cells measured on 2026-09-12) changes the run partitioning and displaces every later section: generations 2 and 3 differ by about 1.1 MB, 3 and 4 by 1.0 MB, and the byte fixpoint arrives only at generation 4 (habu-make-the-engine-9db99082). Acceptance: the capture resets the provably dead tails of persisted stores (the AOT analogue of snap-lib.f SND-ZERO-DEAD-HEAP; which tails are dead is a checker-ownership decision: state it per store), the chain's byte assertion moves to the (2,3) pair and the tool drops back to three builds, and image size no longer pays for dead bytes (report the size before and after). Files: src/habu/aot-capture.f, src/core/checker.f (store bounds), tools/two-generation-build.f, docs/bootstrap.md. Verify: two same-host builds byte-identical; the chain tool green at (2,3); test/run.f. Depends: none. Ownership: hazel. Claim: unassigned.
