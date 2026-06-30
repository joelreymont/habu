---
title: Read-once gradient witness effect
status: closed
priority: 2
issue-type: task
created-at: "2026-06-30T09:17:12.577324+02:00"
closed-at: "2026-06-30T09:50:17.476297+02:00"
close-reason: "Implemented checked once-space witness locally: space-global-once spans/matrices, LOAD-ONCE/STORE-ONCE and ROW-LOAD-ONCE/ROW-STORE-ONCE, AD VJP mappings, negative/positive checker fixtures, PTX text proof via tools/ptx/once-cg.f. Proof: PTX static suite ok; saxpy text suite ok; typed-local/trust/dot/stale-status/host/filemap lints ok; full local suite 24805ms internal / 26.905s wall. Zed/device proof remains under existing device dots."
---

Long-term refinement after conservative scatter-add default. Add a checked read-once/affine gradient-buffer witness so an AD-generated LOAD adjoint may lower to plain STORE only when single-writer/read-once is statically proven. Until this lands, SCATTER-ADD/ROW-SCATTER-ADD remain the default. Verify with negative checked fixtures rejecting plain-store without witness and positive fixtures permitting plain-store only with the witness; device proof remains zed-owned.
