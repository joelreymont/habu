---
title: Build native probes in owned storage
status: active
priority: 2
issue-type: task
created-at: "\"2026-10-15T07:54:35.879097+03:00\""
---

Saved-product RUN-IMAGE probe in test/compiler/native-build-edit-e2e.f now throws E-STR-CAPACITY before process capture: generated source exceeds the shared1024-byte SB. Main proved the failure without rebuilding the engine. Build this fixture script in its own appropriately sized/growing public buffer, preserving complete runtime/private/public origin assertions. Reuse the saved baseline product for probe validation before another cold engine build; no global SB capacity increase, truncation, general harness framework or weakened assertions. Carl engine_shared_core_max owns this test-only correction under59200861, with proportionate real product probe and independent source review.
