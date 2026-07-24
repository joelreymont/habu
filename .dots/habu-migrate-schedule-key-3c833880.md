---
title: Migrate schedule key record
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-24T13:32:31.074689+02:00\""
---

Why: maki/sched-key.f still uses legacy PRODUCT for the typed schedule identity consumed by model optimization and kernel replay. Owner: maki/sched-key.f and maki/sched-key-test.f only. Replace PRODUCT skey directly with STRUCTURE inside public MAKI, preserving DERIVE eq, all nine field names/nominal schemas/order, MAKI-SKEY:MAKE and MAKI-SKEY:EQ spelling/effects, nine-cell layout, semantic-field role rejection, field-equality versus durable-text identity, replay/cache behavior, errors, allocation, and public API. Retarget product comments. Forbidden: aliases, legacy parser edits, raw casts, durable key format changes, replay redesign, field/schema changes, copied tests, or unrelated cleanup. Pre-change proof: the source has exactly one executable PRODUCT and the real suite already proves every semantic field and wrong-role reject. Acceptance: sched-key production suite passes before/after with construction, DERIVE equality discriminating each field, wrong-role checker negatives, shape buckets, render equality, cache replay and reset; exact reflection/effects/layout stable; no executable PRODUCT remains; focused typed-local/package/trust and owning Maki gates pass.

Claim: agent=codex-sched-key-structure workspace=.jj-ws/habu-migrate-schedule-key-3c833880
