---
title: Add WSTORE resident handle for model embedding
status: closed
priority: 2
issue-type: task
created-at: "2026-07-26T14:02:43.278802+02:00"
closed-at: "2026-08-02T15:42:53.250709+02:00"
close-reason: "Ancestor 5b0ebb070a5b deleted the unused GPT2LOAD/GPT2TX/WSTORE/MODELPROV host path and suites; retaining this task would resurrect deleted architecture."
---

Interface leaf ordered before both bind-commit leaves, from the S6b3 checkpoint stop (measured: a STRUCTURE or ENUM field may hold a bare DEFLINEAR but not a compound transitively containing one - structure-decl.f:222 and enum-decl.f:255 via TFAM-CONCRETE-LINEAR? at type-family.f:1663 - so gpt2-model cannot hold WSTORE:store directly, and the alternative of erasing the store into cells inside GPT2TX would hard-code WSTORE's 3-cell layout behind five trusted rows, the exact value-assumption shape the Fix Review Gate rejects). Behavior, in package WSTORE where the layout knowledge belongs: block-backed DEFLINEAR resident (single-cell linear handle owning a store); HOLD ( WSTORE:store -- WSTORE:resident ); RESIDENT-DISPOSE ( WSTORE:resident -- result<n,n> ) totally releasing the underlying store through the existing DISPOSE path. Any internal erasure stays package-private, audited, refine-seeded, citing the linear-scope dot. Tests: hold-then-dispose with WSTORE:LIVE plus SAFET counter deltas for both store arms (mapped and allocated); double-use linearity negatives; ambient access negatives; a with-slot access THROUGH a held resident is NOT required (the model reads weights via S6c later; if trivially exposable note it, else record as S6c work). Acceptance: weight-store suite green; both diff lints; refine-lint green; maki/test.f green. Owner: package WSTORE. Claim: agent=s6b3 workspace=.jj-ws/habu-s6b3-mapped (first commit of the resumed S6b3 lane).
