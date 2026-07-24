---
title: Migrate artifact envelope records
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-24T13:32:31.089721+02:00\""
---

Why: maki/db/artifact.f owns model/kernel artifact intake used by the inference packaging path but still declares weight-artifact, kernel-artifact, and content-digest with legacy PRODUCT. Owner: maki/db/artifact.f and maki/db/artifact-test.f only. Replace those three declarations directly with STRUCTURE inside public ARTIFACT, preserving exact names/schemas/order, generated ARTIFACT-WEIGHT--ARTIFACT, ARTIFACT-KERNEL--ARTIFACT, and ARTIFACT-CONTENT--DIGEST constructor packages, one/one/four-cell layouts, distinct nominal identities, envelope codec/digest/validation/wire behavior, errors, allocation, and public API. Leave art-result SUMTYPE untouched. Retarget comments. Forbidden: aliases, legacy parser edits, raw casts, outcome or codec migration, wire/tag changes, duplicated schema, copied validator, or unrelated cleanup. Pre-change proof: token-aware census finds exactly three executable PRODUCT declarations. Acceptance: the real artifact suite exercises all generated pairs through encode/decode/validate, distinct kind rejection, digest equality/mismatch, canonical and hostile wire fixtures before/after; exact reflection/effects/layout stable; no executable PRODUCT remains; focused typed-local/package/trust and owning Maki gates pass.

Claim: agent=codex-artifact-structure workspace=.jj-ws/habu-migrate-artifact-envelope-96bbc494
