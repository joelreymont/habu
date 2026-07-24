---
title: Migrate built witness record
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-24T13:34:14.379135+02:00\""
---

Why: maki/typestate.f still uses legacy PRODUCT for the unforgeable ART:built witness that every evidence and promotion path consumes. Owner: maki/typestate.f and maki/typestate-test.f only. Replace PRODUCT built directly with STRUCTURE inside public ART, preserving art/build-proof schemas and order, ART-BUILT:MAKE/UNMAKE spelling, two-cell layout, private proof-token minting, BUILD transition, downstream evidence/promotion effects, nominal unforgeability, errors, allocation, and public API. Retarget product comments. Forbidden: aliases, legacy parser edits, raw casts, proof-token exposure, transition redesign, downstream caller edits, copied tests, or cleanup. Pre-change proof: exactly one executable PRODUCT exists and raw/foreign proof construction already rejects through the real checker. Acceptance: typestate production suite passes before/after with BUILD, MAKE/UNMAKE consumers, private-mint visibility and raw/foreign-role negatives; evidence/promotion smoke remains green; exact reflection/effects/layout stable; no executable PRODUCT remains; focused typed-local/package/trust and owning Maki gates pass.

Claim: agent=codex-built-structure workspace=.jj-ws/habu-migrate-built-witness-b98cd3b8
