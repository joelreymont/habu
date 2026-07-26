---
title: Reject bad pointers at ENUM close
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T22:41:07.316830+02:00"
blocks:
  - habu-add-shared-family-76a761c3
---

ENUM integration of the shared query: same rule after final variant, field, and width binding and before layout and generation - call FAMILY-SCHEMA:BAD-PTR, arm DECL-REJECT at the returned payload name, reject, prove rollback and a clean successor declaration. Production-declarer negative for the elaunder reproduction with exact payload-name diagnostic. LAND TOGETHER with the STRUCTURE integration. Acceptance: enum-decl-suite green; both diff lints.

Note (2026-07-26): the symmetric-authority fix lands with the query leaf, so this integration asks TFAM-CONCRETE-LINEAR?-rooted BAD-PTR at ED-CLOSE directly - no participant reordering, no second predicate.

Amended (codex preflight 5): this integration explicitly DELETES the early REQUIRE-POINTEE authority from enum-decl.f, matching the STRUCTURE integration.
