---
title: Remove audited internal machinery
status: active
priority: 1
issue-type: task
created-at: "2026-10-16T11:16:12.196219+03:00"
---

Joel requested whole-repository overengineering fixes and dots, with parallel implementation and Carl/Jerry coordination. Mark integrates isolated commit-sized fixes, meaningful existing behavior tests, required independent Astra reviews, native/convergence checks where impact requires them, and workspace retirement. This family covers new findings; reuse existing residency8b4d1ada, typed-store087ee3fa, capture40efb7d9, tree-shaker720c1e8b, JSONd96fd4bf, and fixtures196bcc0a tasks with their owners. Preserve actual external artifact/cache integrity, runtime lifetimes, and Carl owned native FILE contracts. Source freshness changes concern LSP/index invalidation rather than replacing consumed-input build keys. Close only after every applicable finding is fixed or refuted with evidence and completed changes land.
