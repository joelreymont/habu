---
title: "V2 types: refined CAD numeric roles design"
status: active
priority: 2
issue-type: task
created-at: "2026-07-11T10:25:02.729815+02:00"
---

Problem: MODEL-CAD-V2-PLAN.md:489-504 identifies same-cell length/count/index/offset/alignment/divisor/product gaps in object encoding, bufferization, and shape arithmetic. Bounded design/probe dot under 30 minutes. Fix: specify checked constructors and evidence for nonnegative lengths, bounded indexes, byte/cell offsets, alignment, nonzero divisors, and overflow-checked products; reuse nominal roles and avoid full arithmetic refinement. Acceptance: swapped-role, negative, overflow, misalignment, zero-divisor, and bound fixtures plus migration owners. Files: MODEL-CAD-V2-PLAN.md:489-504, src/core/roles.f, src/core/checker.f, lib/string.f, lib/vector.f, lib/memory.f, maki/model-ir.f. Verify: scalar checker fixtures and owning library tests.

Claim: agent=numeric-roles workspace=.jj-ws/habu-v2-types-refined-519fd2d1.
