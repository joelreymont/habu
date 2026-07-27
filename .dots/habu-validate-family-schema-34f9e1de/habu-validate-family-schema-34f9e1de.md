---
title: Validate family schema after completion
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T22:29:05.163916+02:00"
blocks:
  - habu-add-shared-family-76a761c3
  - habu-reject-bad-pointers-230fa9c9
  - habu-reject-bad-pointers-7c6a5d6e
---

Coordination parent only; it owns no implementation and has no claim.

Why: the parse-time pointer guard reads an incomplete family. Both unified
declarers therefore accept a self-pointer written before a later linear member,
then publish a family that owns linear state through a freely copyable pointer.

The implementation is exactly these three leaves:

- `habu-add-shared-family-76a761c3`: one private, state-free whole-family query
- `habu-reject-bad-pointers-230fa9c9`: STRUCTURE close integration
- `habu-reject-bad-pointers-7c6a5d6e`: ENUM close integration

The two close integrations land together, after the shared query. This parent
closes only after all three reviewed leaves land and the production declaration
gates prove one authority, exact diagnostics, confinement, and byte-identical
rollback.
