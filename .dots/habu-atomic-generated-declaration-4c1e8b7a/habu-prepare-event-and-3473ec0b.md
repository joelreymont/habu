---
title: Prepare event and field release
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T23:20:58.516167+02:00"
blocks:
  - habu-bound-declaration-participant-9967c968
  - habu-make-event-snapshot-0b239a3a
---

Problem: src/core/decl-event.f DEV-FINALIZE and src/core/type-family.f PF-FINALIZE still validate declaration and field tokens and published states after all participants have committed. A stale token or depth mismatch can reject after the event and field high-water marks are public. Exact contract: the field owner exposes one checked prepare-for-release boundary that proves the exact live top token, open state, and frame depth before commit without mutation. DECL-EVENT PREPARE proves its exact live token, open state, event-to-field contiguity, and the field release precondition. COMMIT only advances the reversible event and field publication marks and changes their frame states. The final callbacks then only decrement the already-proved event and field frame depths; they contain no throw, validation, allocation, lookup, or publication. Preserve nested transactions, token freshness, and outer-only published high-water behavior. Package or namespace owner: DECL-EVENT and the current product-field transaction owner in src/core/type-family.f, later re-pointable to TYPE-FIELD without a forwarding shim. Acceptance: production GENERATED-DECL:RUN regressions inject a stale event token, stale field token, wrong frame state, and wrong depth through narrow private test swaps; every case rejects in PREPARE before commit and rollback restores exact event and field bytes. Clean nested success releases exactly one event and field frame. Mutations removing any prepare check, retaining a rejecting final callback, or changing outer publication order must fail. Files: src/core/decl-event.f, src/core/type-family.f, test/decl-event-suite.f, shared candidate-validation enrollment only if needed. Verify: declaration-event production suite, field rollback suite, generated-declaration transaction suite, shared candidate validation, and exact typed-local/package/trust checks. Depends: habu-bound-declaration-participant-9967c968 and habu-make-event-snapshot-0b239a3a. Ownership: event and field release readiness only. Claim: unassigned.
