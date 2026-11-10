---
title: A top-level is of a defer is refused as undefined
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T14:53:31.171106+02:00"
---

Problem: at top level "' E is D" is refused with E-UNDEFINED-TOP-LEVEL at 'is' under bin/hb tools/check.f --verify-only --json-errors (/Users/joel/Work/reviews/habu-10x-2026-11-10/cases8/defer/b-defer.f). Not yet known: whether the load accepts the same line, and whether a top-level is is meant to be written at all; docs/forth-card.md is the reference. Found by the deferral census (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/deferral-census.md, out-of-scope findings). Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: the check and the load answer alike for a top-level is; if it is not allowed the refusal's reason says so and the suggestion names what to write (set the defer inside a definition), else the check accepts it; a test pins it. Files: src/habu/verify-source.f, src/core/checker.f, test/diag-position-test.f. Verify: b-defer.f under the load, the check and --verify-only. Depends: none. Ownership: the source pre-pass. Claim: unassigned.
