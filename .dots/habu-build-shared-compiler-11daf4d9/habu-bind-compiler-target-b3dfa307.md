---
title: Bind compiler target policy
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T22:54:38.097168+02:00"
blocks:
  - habu-add-compiler-ir-21e976fc
  - habu-type-dsl-prove-93da83c4
---

Full context: design sections 5.4-5.5 require explicit immutable target contract and numerical policy before table-bearing compiler stages. Define canonical STRUCTURE/ENUM records for architecture, ABI, features, endianness, pointer width, integer overflow, floating semantics, contraction, fast-math, and comparison policy, with deterministic digests. Acceptance: missing/illegal combinations reject; equal policies digest identically; every semantic field changes identity. Dependencies: compiler IR IDs and the unified type-DSL hard-cutover proof.
