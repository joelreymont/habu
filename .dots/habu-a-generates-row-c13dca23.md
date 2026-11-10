---
title: "A generates: row's claim is not checked against what its definer does"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T16:21:44.190462+02:00"
---

Problem: the source pre-pass trusts a `generates: D ( effect )` row and the load does not check it: a D that reads its name on one path only (o1-o4) keeps its row, and a row with a wrong effect (w1-wrong-effect) gives --verify-only rc 0 where the load refuses E-MISMATCH rc 70. Programs and table in /Users/joel/Work/reviews/habu-10x-2026-11-10/cases8/genrow; hunt /Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/genrow-hunt.md (What the fix leaves open). Checking the claim needs a check at the load's top-level call, which lives in the arm64 interpreter (src/habu/habu1.f, habu2.f), machinery AGENTS.md says is being replaced and must not be patched. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: this is an acceptance case of the dot that replaces that interpreter (one typed interpreter on every machine): o1-o4 and w1 agree between --verify-only and the load. Files: none until the replacement. Verify: the genrow table's o and w rows. Depends: the one-interpreter replacement. Ownership: the source pre-pass. Claim: unassigned.
