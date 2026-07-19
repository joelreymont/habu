---
title: "Raise MODEL: named-ref queue cap (CAP-PEND-CAP)"
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-19T23:49:05.195020+02:00\""
---

Found by the GPT-2 block lane 2026-07-19 (falsified live): cad.f CAP-PEND-CAP=4 bounds the named-reference queue PER BODY (only CAP-BEGIN resets it), and a 5th named ref throws E-CAD-REF (-5029). A GPT-2 block body needs 5+ (pre-LN affine 2 + residual 1 + final affine LN 2), forcing the final LN to be composed from primitives that auto-drain the input cursor instead of the fused arity-3 op. Fix properly: either raise the cap to a justified bound (count worst-case refs in a realistic block: Nx blocks each 2 LN x 2 params + residuals -> derive, do not guess) or make the queue drain per-consumption so the cap bounds OUTSTANDING refs not total refs (read CAP-PEND semantics first - which is architecturally right depends on whether refs are consumed at emit; state the evidence). Both-direction regression: a body with the new max passes, one past rejects E-CAD-REF. Territory: maki/cad.f + a capture test. NOTE: gptblock-test.f's composed-LN workaround stays valid either way; do not churn it in this lane.

Claim: agent=capraise workspace=.jj-ws/capraise machine=spark
