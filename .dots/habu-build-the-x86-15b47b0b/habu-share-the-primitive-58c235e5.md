---
title: Share the primitive registry and completeness gate
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.542981+03:00"
---

Problem: registration (`src/habu/habu1.f:109-155 FP-ARGS`) and the completeness gate (`habu2.f ENGINE-EMIT:EMIT-PRIMITIVE-SECTIONS`) live in the ARM64 builder.
Acceptance: both live in `src/habu/primitive-registry.f`, used by `habu1.f` and by `kernel-x64.f`; the ARM64 engine byte-identical (chain).
Files: `src/habu/primitive-registry.f`, `src/habu/habu1.f`, `src/habu/habu2.f`, `src/habu/treeshake.f`.
Verify: spark: rebuild; chain gen2==gen3 byte-identical to master's engine; gate.
Depends: none. Serialise on `habu2.f` with I6, I10c and X7.
Route: Alder (shared: src/habu/primitive-registry.f, src/habu/habu1.f, src/habu/habu2.f, src/habu/treeshake.f).
Ownership: krait (Intel lane).
Claim: unassigned.
