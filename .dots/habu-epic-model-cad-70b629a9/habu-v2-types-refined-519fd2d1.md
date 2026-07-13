---
title: "V2 types: refined CAD numeric roles design"
status: active
priority: 2
issue-type: task
created-at: "2026-07-11T10:25:02.729815+02:00"
---

Problem: MODEL-CAD-V2-PLAN.md B5 identifies same-cell scalar length/count/index/offset/alignment and allocation gaps. Design scope is scalar-only: no bounded-index/aligned-size evidence, signed divisor, general dependent arithmetic, checker implementation, or direct consumer edits. Acceptance: specify exact nominal roles and zero semantics, specialized numeric-result<a>, the closed dimensionally valid arithmetic table and boundary matrix, MEM-private primitive projections, package-first memory/string/vector/MIR contracts, exhaustive caller censuses, non-overlapping implementation owners, final CAD-NUM sealing, and shape-census ownership. Implementation leaves: habu-implement-cad-num-962bf5d9, habu-implement-cad-num-cb413b2a, habu-seal-cad-num-36dbeec6, the memory/string/vector/MIR owner and caller dots, habu-census-lower-shape-836287e4, habu-census-legacy-str-b84390fe, and habu-integrate-sealed-cad-ba510e2e. Files: MODEL-CAD-V2-PLAN.md only. Verify: adversarial design review plus host/filemap/dot/status lints.

Claim: agent=numeric-roles workspace=.jj-ws/habu-v2-types-refined-519fd2d1.
