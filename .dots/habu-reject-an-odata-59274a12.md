---
title: Reject an OData key no key predicate can write
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T01:28:38.913069+01:00"
---

Problem: ODATA:METADATA accepts an entity type whose Key names a property of a type ODATA:KEY cannot write for the document's version (a V2 Edm.Date, Edm.TimeOfDay or Edm.Duration key; any non-primitive key), so KEY fails at run time with E-KEY on every entity of that type (src/lib/odata/key.f LITERAL's last FITS; odata-test.f V2-KEYS). The check belongs in METADATA, where the document is read. Acceptance: METADATA rejects such a Key with E-METADATA text placed at the PropertyRef; KEY's no-row branch is gone; a test pins the text. Depends: none. Claim: unassigned.
