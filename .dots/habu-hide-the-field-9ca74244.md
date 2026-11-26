---
title: Hide the field store from programs
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T16:55:02.634424+01:00"
---

Problem: TFAM's field store words are public (src/core/type-family.f: PF-REWIND, PF-N, PF-COMMIT-N, PF-ROW, PF-LINK, PF-ENSURE, TF-INTERN, TFM-EPOCH+, PF-NAME-REQUIRE, PF-DUP?, PF-LAYOUT-REQUIRE, PF-OVERLAP?, PF-SCHEMA-OK?, PF-OWNER-OK?, and PF-ADD/PF-COMMIT after 080dd4ac), so a program can write or rewind field rows: `' TFAM:PF-REWIND` resolves and a call returns rc 0 (hunt W71b). TYPE-DECL's declaration code reaches them through `using TFAM`, so neither a private section nor a part of TYPE-DECL hides them. PF-ADD's dies (owner, schema, overlap) guard only program input: DECL-EVENT FIELD already rejects a bad schema with E-PF-SCHEMA. Acceptance: no program reaches TFAM's field store words; PF-ADD's checks of what its engine callers hold are deleted. Depends: habu-keep-field-txns-080dd4ac. Claim: unassigned.
