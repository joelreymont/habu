---
title: Place a bad field type at the type, not the field name
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T13:29:30.087488+02:00"
---

Problem (dot c9fe6f17 census, 'found on the way'): a STRUCTURE field whose type is unknown (FIELD items nosuch) is refused E-BAD-DECLARATION 'unknown field type' placed at the field name, not at the unknown type token (src/core/structure-decl.f:215). Acceptance: the packet's token and place are the type token; a test case asserts it. Files: src/core/structure-decl.f. Verify: tools/check.f --json-errors on a one-line STRUCTURE with FIELD items nosuch shows token nosuch and its column. Depends: none. Ownership: structure-decl.f. Claim: unassigned.
