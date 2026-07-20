---
title: Equation registry rank-0 outputs (auto-derived scalar adjoints)
status: open
priority: 2
issue-type: task
created-at: "2026-07-20T13:04:58.890511+02:00"
---

Wall documented at the rank-0 accessor landing (e318dadf): the AUTO-derived adjoint of a scalar broadcast is itself an equation with a RANK-0 OUTPUT (s[] = O[m n] . A[m n] +SUM m n), and the equation registry rejects rank-0 on both sides - EQ-FAC-PLAIN? refuses rank-0 factors and SP-VALIDATE-CT requires >=1 free index - so SPEC-ADJ-CHECK$ on a scalar form fails closed named (E-SPEC-ARITY; never a wrong gradient) and scalar-broadcast training currently uses AUTHORED analytic adjoints (gradchecked, landed). Fix at stage-1 equation-registry level: admit rank-0 outputs (a full-reduction equation with an empty free list) and rank-0 factors through EQ-FAC-PLAIN?/SP-VALIDATE-CT with the derived-adjoint pipeline handling the empty-free case; then the scalar form's auto-adjoint derives like every other and the authored-adjoint special case in the tests retires. Red-first: the current named reject is the baseline; after, the derived adjoint must gradcheck against the same central-FD reference the authored one passes. Territory: maki/spec.f validators + equation registry, spec tests.
