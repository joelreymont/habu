---
title: Specify PTX generated-state integrity
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-13T15:01:03.173380+02:00\""
blocks:
  - habu-v2-types-artifact-6ee556f8
---

Full context: MODEL-CAD-V2-PLAN.md R9 promises independently verifiable native and device code generation, but every named machine-state implementation slice is ARM64-specific while PTX virtual-register allocation, predication, address spaces, reconvergence/barrier legality, resource declarations, and the ptxas-to-cubin/SASS boundary have no complete owner. Existing PTX IR, mask/barrier, resource-model, device-golden, and proof-carrying dots cover pieces and must not be duplicated. Fix: perform a fixed-file/call-graph census from typed PTX IR and emitters through ptxas assembly, cubin identity, launch, evidence, and promotion; specify the target-indexed virtual machine-state contract and the exact independent proof boundary for proprietary ptxas allocation/scheduling; map existing dots and create only missing disjoint implementation leaves with dependencies. Acceptance: the design distinguishes host Forth stack effects, PTX virtual def/use and control/resource effects, ptxas resource/allocation attestations, SASS/cubin identity, and device semantic evidence; undefined use, duplicate def, predicate/control mismatch, divergent barrier, address-space/type mismatch, stale toolchain, resource-report drift, cubin mutation, and unsupported unverifiable backend all fail closed at a named owner; every existing overlapping dot is reused exactly once; R9 tracked slices and V2 Definition of Done name the resulting leaves; dot-dep, host, filemap, and status lints pass. Files: MODEL-CAD-V2-PLAN.md, docs/ptx.md, tracker records only; no compiler, emitter, or device source edits. Verify: adversarial design review against lib/ptx, src/arch/ptx, maki lowering/promotion, ptxas/device gates, and dependency lint.
