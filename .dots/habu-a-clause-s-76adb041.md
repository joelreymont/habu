---
title: "A clause's tick deferral hides its parent's deferral"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T14:53:31.182083+02:00"
---

Problem: for a definer whose parent body is deferred for a rendered name (MADE) and whose does> clause is deferred for a tick, --verify-only writes one W-CHECK-DEFERRED packet, the clause's tick, and none for the parent (/Users/joel/Work/reviews/habu-10x-2026-11-10/cases8/defer/d-tick-clause2.f; src/habu/verify-source.f REPORT-DEFERRED sites :1806-1842). An agent that lifts the tick still has a deferred definer and was not told. Found by the deferral census (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/deferral-census.md, out-of-scope findings). Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: each deferred body of a definer, parent and clause, gets its own packet at its own token; a test pins d-tick-clause2.f with two packets. Files: src/habu/verify-source.f, test/diag-position-test.f. Verify: d-tick-clause2.f under bin/hb tools/check.f --verify-only --json-errors. Depends: none. Ownership: the source pre-pass. Claim: unassigned.
