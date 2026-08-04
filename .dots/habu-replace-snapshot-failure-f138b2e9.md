---
title: Replace snapshot failure whitebox
status: open
priority: 2
issue-type: task
created-at: "2026-07-24T20:49:38.693056+02:00"
blocks:
  - habu-move-field-token-bd9b0350
---

Why: section 14 of `test/decl-event-suite.f` mutates `PF-TX-SERIAL` and
reads or restores private declaration-event and field-owner storage, so sealing
the owner invalidates the proof.

Result and owner: one focused checked-Habu child-engine mutation test plus the
necessary removal from `test/decl-event-suite.f`. A disposable child changes
only the field transaction next-open condition so the real
`GENERATED-DECL:RUN` event snapshot fails with `E-PF-TX` 7123. Observe only
public event count, identity, depth, field count, field depth, and subsequent
behavior.

Acceptance: failure returns exactly 7123; every public observation equals its
pre-run baseline; a clean generated declaration immediately afterward matches
an independent clean child; moving any event write before the field snapshot
makes the test fail. Delete `TEST-PF-SWAP`, the B/C/S field snapshot helpers,
private DEV snapshot helpers, and their source trust assertions. Do not add a
production setter, hook, bridge, package reopen, copied transaction, direct
private read, host automation, or pre-comparison restore. Run the focused child
test, generated-declaration transaction suite, and declaration-event suite.
Depends on the field-token authority tests. Claim: unassigned.
