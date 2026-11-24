---
title: "Fold JSON Schema's piece into its lane"
status: open
priority: 2
issue-type: task
created-at: "2026-11-24T19:40:41.752940+01:00"
---

Problem: src/lib/json-schema.f creates every piece in NEW-LANE as that lane's first piece (about :772-777, :850), so pieces and lanes are one to one; the piece's lane field only leads back to its lane. Acceptance: lanes chain directly with the piece's next, prev and text; the piece structure, the PIECES arena, LANE-FIRST and the back-pointer are gone; json-schema and mcp suites pass with the same texts. Depends: none. Claim: unassigned.
