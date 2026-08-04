---
title: Isolate field validation regressions
status: open
priority: 2
issue-type: task
created-at: "2026-07-17T12:12:18.906142+02:00"
blocks:
  - habu-factor-field-schema-f4d27285
---

Test review findings in test/type-family-suite.f:445,625,643: optional-owner negative combines product-kind and variant-owner failures; overlap negatives combine logical and byte overlap, so independent checks could disappear; CUSTOM provider ordering is asserted for only the combined case. Fix: add a second valid sum/enum family and cross-family variant mismatch, invalid negative and out-of-range variant IDs; add logical-only and byte-only overlap rejects for built-in and CUSTOM policies; prove provider call count is unchanged for each pre-provider reject. Keep fixtures factored into named checked test words rather than duplicated raw stacks. Acceptance: each invariant has one discriminating negative and one positive control; type-family suite, prop test, typed-local lint, full gate green. Files: test/type-family-suite.f.
