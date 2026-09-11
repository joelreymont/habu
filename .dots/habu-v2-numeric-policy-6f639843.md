---
title: V2 numeric policy schema
status: closed
priority: 1
issue-type: task
created-at: "2026-07-11T12:14:25.421270+02:00"
closed-at: "2026-07-14T23:52:52.327838+02:00"
close-reason: "Merged 710eed8f on master: maki/numpolicy.f - NPOL:dom ENUM (exact/ulp/relative/empirical, DERIVE eq), RANK strength ordinal, SATISFIES?/ENFORCE (E-NPOL-APPROX named refusal), COMPOSE weakest-wins (4x4 table pinned; commutative/associative/idempotent), NUM>DOM bridge from op-registry per-op tags, ambient POL! requested policy. Threaded into the EXACT KEY: skey gained FIELD pol, SK-KEY$ renders per-region policy - key-invalidation proven (SK-EQ-POL false, render changes under POL!, store/replay NO-PAIRING test: exact-key selection misses under relative key); EVID:golden gained FIELD pol (GOLD-DOM projection) so promotion rows carry policy via SK-KEY$. Executed refusals: TF32-vs-FP32, GELU, recompute positive+negative each; golden-record level over a real pipeline (E2E-GOLD-EXACT-REFUSE throws, REL-OK passes). All fixtures + full maki suite (105 suites) + lints green on the exact merged tree. Stale file list adapted to landed R7 reality (no maki/promotion.f; golden record = evidence/schema.f; key = sched-key.f) - policy axis kept distinct from the precision dtype axis by design. Follow-on dotted: wire ENFORCE into the POLICY:CHECK gate-set. Unblocks bench schema 5341ffc8."
blocks:
  - habu-v2-research-approximation-c10e7cc6
---

Problem: MODEL-CAD-V2-PLAN.md:1471-1482 requires precision in every plan/artifact key; current flags can silently compare FP32 FMA and TF32 tensor-core results. Fix: implement exact/ULP/relative/empirical policy values and attach them to rewrite, schedule, golden, and promotion records. Acceptance: approximate evidence cannot satisfy exact policy; deterministic composition is tested; changing policy invalidates plan/artifact/tuning keys. Files: maki/precision.f, maki/golden.f, maki/schedule.f, maki/promotion.f. Verify: TF32/GELU/recompute positive and negative fixtures.

Claim: agent=numpolicy workspace=.jj-ws/fable-numpolicy
