---
title: Derive adjoint equations for einsum ops
status: closed
priority: 1
issue-type: task
created-at: "2026-07-19T11:05:59.543417+02:00"
closed-at: "2026-07-19T18:57:56.412293+02:00"
close-reason: "Landed 3c21a234: EQ-ADJ-DERIVE at SPEC: through same parser/emitter, BW-STEP-EQUATION single arm, fd-checked vs central-FD oracle, equation-op GEMM trains bit-identically to matmul kind on adam-train fixture (40 steps, 0 mismatches), gather -> named E-CAD-GRAD at derivation boundary"
---

Stage 2 of docs/model-unified.md. The adjoint of an einsum is another einsum: dFj's equation has Fj's indices as output, factors = dO plus every other Fi, summed over indices not free in Fj. Generate the adjoint equations at declaration time through the SAME parser/emitter (they are ordinary equations); backward.f gains ONE equation arm running the pre-derived adjoints. Every derived adjoint is finite-difference-checked by the adam-train gradient harness before the kind trains. Gathers: adjoint is scatter-add, not expressible - a gather equation registers forward-only and REJECTS under training with named E-CAD-GRAD (never a wrong gradient); scatter-add primitive is its own follow-up dot (embedding lookup needs it). Acceptance: equation-op GEMM trains identically to the matmul op-kind on the adam-train.f:224 fixture.
