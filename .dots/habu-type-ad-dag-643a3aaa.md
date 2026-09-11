---
title: Type AD DAG identities
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T21:05:59.144519+02:00"
blocks:
  - habu-type-ad-dag-644833ca
---

lib/ptx/ad-dag.f represents four different roles as interchangeable n cells: DAG node identities (AD-A/AD-B/AD-VS/AD-OUT), table indexes and counts, PTX register numbers (AD-REG), and optional cotangent registers (AD-CT). AD-A/AD-B use -1 as an absent-node sentinel (:113-134); AD-REG/AD-CT independently use -1 as absent-register sentinels (:76-81, :174-181). Public/private effects such as AD-VPUSH/POP/TOP, AD-NODE, AD-A@/AD-B@/AD-REG@/AD-CT@, AD-ACC, and every emitter all accept/return n, so swapping a register number for a node id certifies and can index the wrong table before runtime bounds happen to reject. The landed TYPEFAMILY, option, LAYOUT-BUFFER, and TYPED-VARIABLE facilities can express these roles now. After operation-domain typing, add package-owned nominal node-id; make the value stack, child columns, and AD-OUT typed storage; replace -1 child sentinels with option<node-id>; keep registers as n (or a distinct existing register family if one exists) and replace register -1 sentinels with option<n>. Centralize the sole bounds-validated raw-index<->node-id authority inside the owning package; do not expose a raw projection or add unchecked generic storage. Rewrite effects/locals so node/register/index swaps reject statically, and use MATCH for optional edges/cotangents. Preserve graph order, fan-out accumulation, emitted PTX, host evaluator results, overflow diagnostics, and zero-allocation operation. Add checked negative fixtures for every cross-role swap and stale/out-of-range raw mint; property-test that every returned node id is live and every child precedes its parent; measure CODELEN and DATA before/after with no unexplained growth. Files: lib/ptx/ad-dag.f, ad-dag-eval.f, their tests and direct tool callers. Depends: habu-type-ad-dag-644833ca (and its package dependency). Ownership: DAG identity/optional-reference typing only.
