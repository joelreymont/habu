---
title: V2 evidence applicability checker
status: open
priority: 1
issue-type: task
created-at: "2026-07-11T12:25:27.265401+02:00"
blocks:
  - habu-v2-proof-obligation-6cf70b4f
---

Implement obligation closure and evidence applicability over exact subject/dependency/schema/target/numeric/verifier/environment digests. Produce typed stale/missing/inapplicable results and the minimal invalidation set. Acceptance: mutation matrix pins each key component, static proof cannot satisfy required device execution, performance evidence cannot satisfy equivalence, and cache-hit closure equals uncached closure.
