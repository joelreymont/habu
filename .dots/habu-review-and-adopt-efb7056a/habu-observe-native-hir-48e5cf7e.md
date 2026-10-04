---
title: Observe native HIR after freeze
status: active
priority: 1
issue-type: task
created-at: "\"2026-10-04T05:08:20.586680+03:00\""
---

Problem: HBR2 UI-ADMIT needs one target-neutral callback after folded HIR freeze and before native backend selection; wrapping each architecture’s SELECT replaces a provider row. Requirement: a default no-op observer installed through NBACK sees later tier1 definitions, none at tier0; a throw refuses publication with its original code and preserves cleanup/retry. Determine and document the exact pending-record/published-entry and nested quotation function identity. No UI policy or per-architecture wrapper belongs in the engine. Ownership: dave, implementation .jj-ws/dave-freeze-observer; src/compiler/native/backend.f, compiler.f, observer E2E, gate registration and owning documentation. Acceptance: real tier0/tier1 folded-HIR observation, literal-quotation identity discriminator, throw/retry and callback capture lifetime; independent adversarial Astra/Fable, full native and generation convergence before public release. Design: /private/tmp/claude-501/reports/dave-hbr2-requests.md and adopted HBR2 §7.5. No dependency on a private package build or consumer workaround.

The callback CODE cells occupy $2CF0, $2CF8 and $2D00 in both native and seed images, leaving $2CE8 for the literal store's DATA floor. Combined runtime qualification remains required before release.
