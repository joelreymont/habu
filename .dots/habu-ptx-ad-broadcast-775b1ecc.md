---
title: "PTX AD: BROADCAST primitive"
status: open
priority: 2
issue-type: task
created-at: "2026-06-26T23:59:59.013630+02:00"
blocks:
  - habu-ptx-m6-collectives-12cf0e2d
---

New M6-era primitive. autograd.md: BROADCAST ( uniform<f32> -- tile<f32,B,M> ) is the named form of the implicit broadcast already inside B-/B/, and the type-dual (mutual adjoint) of BLOCK-SUM ( tile -- uniform ). The ONE rename needed to make SOFTMAX-ROWS-BWD buildable.
- Files: lib/ptx.f or src/arch/ptx/ (the tile vocabulary); lowering = fill all active lanes with the uniform value, inactive lanes poison per the mask M.
- Verify: BROADCAST then BLOCK-SUM round-trips identity-ish on a full mask; type-checks as ( uniform -- tile ); mask token threads. T{ }T.
- Dep: M6 (habu-ptx-m6-collectives-12cf0e2d). Consumed by ad-reverse (habu-ptx-ad-reverse-26aebee3) VJP: BLOCK-SUM.
