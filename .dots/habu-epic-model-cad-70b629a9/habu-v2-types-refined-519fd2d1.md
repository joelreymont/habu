---
title: "V2 types: refined CAD numeric roles design"
status: open
priority: 2
issue-type: task
created-at: "2026-07-11T10:25:02.729815+02:00"
blocks:
  - habu-reconcile-cad-num-64ea7c07
  - habu-reconcile-cad-num-9af0abc0
  - habu-census-lower-shape-836287e4
---

Problem: MODEL-CAD-V2-PLAN.md B5 identifies same-cell scalar length/count/index/offset/alignment and allocation gaps. Design scope is scalar-only: no bounded-index/aligned-size evidence, signed divisor, general dependent arithmetic, checker implementation, or direct consumer edits. Acceptance: specify exact nominal roles and zero semantics, specialized numeric-result<a>, the closed dimensionally valid arithmetic table and boundary matrix, MEM-private primitive projections, package-first memory/string/vector/MIR contracts, exhaustive caller censuses, non-overlapping implementation owners, final CAD-NUM sealing, and shape-census ownership. Implementation leaves: habu-migrate-cad-num-cf178e59, habu-seal-cad-num-36dbeec6, the memory/string/vector/MIR owner and caller dots, habu-census-lower-shape-836287e4, habu-census-legacy-str-b84390fe, and habu-integrate-sealed-cad-ba510e2e. Files: MODEL-CAD-V2-PLAN.md only. Verify: adversarial design review plus host/filemap/dot/status lints. Design text landed; the dot remains open until the required censuses, plan-state reconciliation, and TRUSTED retirement audit are complete.
