---
title: "A parses: row for a defer or a primitive is accepted and ignored"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T14:53:31.160302+02:00"
---

Problem: docs/forth-card.md :240-244 says a defer can be declared with parses:, docs/forth.md :2453 and src/habu/verify-source.f TOP-OPAQUE :3142 say it cannot: a top-level defer stops the check even with 'parses: D 0' (/Users/joel/Work/reviews/habu-10x-2026-11-10/cases8/defer/b-defer-parses.f), and 'parses: parse-name 1' is accepted with no effect and no E-PARSES-ROW (p-prim2.f). An agent that writes the row the card names gets a deferral it cannot lift. Found by the deferral census (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/deferral-census.md, out-of-scope findings). Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: a parses: or parses-through: row naming a defer or a primitive is refused with E-PARSES-ROW at the row, or it bounds the word as the card says; the card and forth.md say the same thing as the engine; a test pins each. Files: src/habu/verify-source.f, docs/forth-card.md, docs/forth.md, test/diag-position-test.f. Verify: b-defer-parses.f and p-prim2.f under bin/hb tools/check.f --verify-only --json-errors. Depends: none. Ownership: the source pre-pass. Claim: unassigned.
