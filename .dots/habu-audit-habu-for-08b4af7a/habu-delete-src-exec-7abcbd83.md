---
title: Delete SOURCE-EXEC-CTX-CELL
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T11:00:44.422638+02:00"
---

Problem: the observer lane kept src/habu/layout.f SOURCE-EXEC-CTX-CELL, its DATA-BANDS row and its claim only because store-guard tests use it as a sealed band cell (test/x86-64-kernel-syscalls.f:33, -engine.f:551, -pure.f:235, -atomics.f:68, -ffi.f:58, test/protection-span.f:108-109). Acceptance: once the store guard is gone, the cell, its rows and those cases go. Depends: habu-delete-the-store-fd3e8182. Claim: unassigned.
