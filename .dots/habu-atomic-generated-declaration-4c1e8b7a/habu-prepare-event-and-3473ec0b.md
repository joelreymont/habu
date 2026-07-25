---
title: Prepare event and field release
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T23:20:58.516167+02:00"
blocks:
  - habu-bound-declaration-participant-9967c968
  - habu-make-event-snapshot-0b239a3a
  - habu-own-product-field-86660116
---

Problem: src/core/decl-event.f DEV-FINALIZE and src/core/type-family.f PF-FINALIZE still validate declaration and field tokens and published states after all participants have committed. A stale token or depth mismatch can reject after the event and field high-water marks are public. Exact contract: after habu-own-product-field-86660116 establishes the sole qualified lifecycle owner, that owner exposes one checked prepare-for-release boundary that proves the exact live top token, open state, and frame depth before commit without mutation. DECL-EVENT PREPARE proves its exact live token, open state, event-to-field contiguity, and the field release precondition. COMMIT only advances the reversible event and field publication marks and changes their frame states. The final callbacks then only decrement the already-proved event and field frame depths; they contain no throw, validation, allocation, lookup, or publication. Preserve nested transactions, token freshness, and outer-only published high-water behavior. Package owners: DECL-EVENT and TYPE-FIELD-OWNER; no raw forwarding shim. Acceptance: production GENERATED-DECL:RUN regressions inject a stale event token, stale field token, wrong frame state, and wrong depth through narrow private test swaps; every case rejects in PREPARE before commit and rollback restores exact event and field bytes. Clean nested success releases exactly one event and field frame. Mutations removing any prepare check, retaining a rejecting final callback, or changing outer publication order must fail. Files: src/core/decl-event.f, src/core/type-family.f, test/decl-event-suite.f, shared candidate-validation enrollment only if needed. Verify: declaration-event production suite, field rollback suite, generated-declaration transaction suite, shared candidate validation, and exact typed-local/package/trust checks. Depends: habu-bound-declaration-participant-9967c968, habu-make-event-snapshot-0b239a3a, and habu-own-product-field-86660116. Ownership: event and field release readiness only.

Claim: released.

Abandoned lane (2026-07-25): the former release_event lane left commit 559c0c0369a3b49d179948f786a6b8a7a4be1105 (change omturzxxtksuqmttkytznxmxsrkpvnsl, described "wip: prepare declaration release") in workspace .jj-ws/habu-release-event. That commit sits on parent 227b5b349702 and is not an ancestor of master@origin, so nothing from it has landed. It touches TRUSTED.md, src/core/decl-event.f, src/core/generated-declaration-protection.f, src/core/type-family.f, test/decl-event-suite.f and test/generated-declaration-transaction-suite.f. Treat it as evidence only: its base predates the whole CHECK package train, so it must not be rebased forward and merged as-is. Whoever takes this dot next starts from current master@origin and may read that commit for the shape of the earlier attempt. The dot's contract and its three dependencies are unchanged.
