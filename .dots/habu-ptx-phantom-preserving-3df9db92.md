---
title: PTX phantom-preserving effects
status: open
priority: 2
issue-type: task
created-at: "\"\\\"\\\\\\\"2026-07-01T23:07:20.878978+02:00\\\\\\\"\\\"\""
---

Invariant: a type-preserving PTX emitter threads the same register phantom
through checked code; a fresh phantom remains a checker-owned mint boundary.

Landed product results:

- Commit `0360af4a` added forge-proof `PTXREP:REP1`, `REP2`, and `REPMIX2`
  combinators in `lib/ptx/rep.f`, converted the first preserving wrappers, and
  pinned forge, kind, and arity rejection in `rep-test.f` and
  `rep-neg-test.f`. No checker change was needed for preserving flow.
- Commit `c2f1c298` added the sink and ternary combinators and converted their
  preserving callers.
- Commit `a2a8386e` added the checker `NP-MINT-CHECK` seal at
  `E-NONPARAMETRIC-EFFECT`: checked code cannot introduce a register-phantom
  family output variable absent from its inputs. It converted the shared
  projection cases and pinned positive and negative checker coverage.
- Commit `b34ac7ae` reused the existing projection boundary for `LOAD-V4`.
  The remaining distinct single-use projections were proven to move, not
  reduce, trust and therefore stayed at their source boundary.

Remaining result: keep this dot as source owner for the actual fresh-mask,
launch-ABI, cp.async typestate, and distinct projection boundaries in
`lib/ptx`. Convert a site only when the current checker structurally proves
the same phantom or a shared projection; never replace one explicit boundary
with a single-use wrapper. Every retained source `TRUST` carries only its
source-local rationale, this retirement owner, and a focused production-path
test. The cp.async subset remains ordered by
`habu-checker-cp-async-6ba788a5`.

Acceptance: `rep-test.f`, `rep-neg-test.f`, the focused PTX emitter tests,
byte-identical output comparisons, PTX standard-library, Maki, fixpoint, and
full native gates pass; free-output loosening, family forgery, wrong kind,
wrong arity, and mask disagreement reject.

Claim: agent=phantom workspace=.jj-ws/fable-phantom (host lane, LEG 1: capability + pilot; lib/ptx/cg-mma.f + cg-matmul.f are FENCED to the wave3 lane - broad sweep is leg 2)

Historical claim; released after commit `0360af4a` landed.

Claim: agent=leg2b workspace=.jj-ws/fable-leg2b (LEG 2B: the checked-mint / rep-provenance capability - owns src/core/checker.f + type-family.f this session)

Historical claim; released after commit `a2a8386e` landed.

Claim: agent=leg2c workspace=.jj-ws/fable-leg2c (LEG 2C: projection-load batch - tile.f/tile-v4.f/tile-v4a.f/tile-smem.f loads; land only if net trust <= -1)

Historical claim; released after commit `b34ac7ae` resolved the batch.
