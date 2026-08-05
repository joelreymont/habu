---
title: Replace snapshot failure whitebox
status: open
priority: 2
issue-type: task
created-at: "2026-07-24T20:49:38.693056+02:00"
blocks:
  - habu-move-field-token-bd9b0350
---

Why: section 14 of test/decl-event-suite.f mutates PF-TX-SERIAL and reads or restores private DECL-EVENT and field-owner storage, so owner sealing makes the proof invalid. Owner: one focused checked-Habu child-engine mutation test, canonical test inventories, TRUSTED.md, and removal from test/decl-event-suite.f. Exact result: a disposable child changes only the field transaction next-open condition so the real GENERATED-DECL:RUN event snapshot fails with E-PF-TX 7123; inspect only public event count, identity, depth, field count, field depth, and subsequent behavior. Acceptance: failure returns exactly 7123; every public observation equals the pre-run baseline; a clean generated declaration immediately afterward matches an independent clean child; moving any event write before the field snapshot makes the test fail. Delete TEST-PF-SWAP, all B, C, and S field snapshot helpers, private DEV snapshot helpers, and the corresponding trust row. Forbidden: production setter, hook, bridge, package reopen, copied transaction, direct private read, host automation, or restoring state before comparison. Smallest checks: the focused child test, generated-declaration transaction suite, and declaration-event suite. Depends: Move field-token authority tests. Claim: unassigned.
