---
title: Read-once gradient witness effect
status: open
priority: 2
issue-type: task
created-at: "2026-06-30T09:17:12.577324+02:00"
---

Long-term refinement after conservative scatter-add default. Add a checked read-once/affine gradient-buffer witness so an AD-generated LOAD adjoint may lower to plain STORE only when single-writer/read-once is statically proven. Until this lands, SCATTER-ADD/ROW-SCATTER-ADD remain the default. Verify with negative checked fixtures rejecting plain-store without witness and positive fixtures permitting plain-store only with the witness; device proof remains zed-owned.
