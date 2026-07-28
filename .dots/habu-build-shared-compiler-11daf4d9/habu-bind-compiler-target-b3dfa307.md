---
title: Bind compiler target policy
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-26T22:54:38.097168+02:00\""
blocks:
  - habu-add-compiler-ir-21e976fc
  - habu-type-dsl-prove-93da83c4
---

Claim: agent=bind_target workspace=.jj-ws/habu-bind-compiler-target-b3dfa307

Dependency review 2026-07-28: the listed prerequisite habu-type-dsl-prove-93da83c4
is NOT a functional blocker and this task proceeds without it. That task is a
landing and verification job for the type-DSL migration ("rebase the integrated
hard-cutover tree, regenerate the exact native fixpoint, prove no legacy definer
survives ... move master only by verified-green fast-forward"). It produces no
capability this task consumes. STRUCTURE and ENUM already work: the enum census
counts 111 plain and 87 full-form declaration sites across the tree, all
compiling, in lib/map.f, lib/build-cache.f, lib/cad-num-types.f and many others.
The edge encoded sequencing hygiene - do not add new type-DSL users before the
legacy surface is proven gone - but this task declares records on the NEW
surface, which helps the cutover rather than hindering it. Do not use any legacy
definer.

Full context: design sections 5.4-5.5 require explicit immutable target contract and numerical policy before table-bearing compiler stages. Define canonical STRUCTURE/ENUM records for architecture, ABI, features, endianness, pointer width, integer overflow, floating semantics, contraction, fast-math, and comparison policy, with deterministic digests. Acceptance: missing/illegal combinations reject; equal policies digest identically; every semantic field changes identity. Dependencies: compiler IR IDs and the unified type-DSL hard-cutover proof.
