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

NOTE 2026-07-17 (diag landing 6b19cda8): when this dot (or the promotion
sibling) lands, mint the EVIDENCE owner package per the plan-23.9 codec
mechanism (evidence-id constructor + refinement pair + ID>WIRE/WIRE>ID) -
the diagnostic IR's invalidated-evidence[] field is waiting on it to be
promoted from string[] to nominal ids (see habu-diag-nominal-ids dot).
