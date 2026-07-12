---
title: "V2 R3: type tensor descriptor kinds"
status: open
priority: 1
issue-type: task
created-at: "2026-07-12T07:09:16.514813+02:00"
blocks:
  - habu-v2-r3-declare-3fcdeebb
---

Problem: maki/tensor.f, tensor-value.f, and descriptor consumers pass dim/shape/dtype/layout/address-space facts as raw n, permitting same-cell swaps. Fix: migrate public descriptor constructors/accessors and checked locals to CAD-KIND nominal kinds; keep arithmetic on validated private projections and return refined results. Acceptance: dim/dtype, dtype/layout, layout/address-space, row/column role swaps reject; broadcast/byte-count happy paths certify and execute; storage round-trips preserve each kind. Files: maki/tensor.f, maki/tensor-value.f, maki/shape*.f and focused consumers/tests. Verify: tensor focused tests, maki/test.f, typed-local diff lint. Depends: CAD kind declarations.
