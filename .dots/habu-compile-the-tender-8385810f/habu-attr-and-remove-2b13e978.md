---
title: Attribute and remove checker work inside the optimizer
status: open
priority: 2
issue-type: task
created-at: "2026-09-11T16:38:06.274143+03:00"
---

Problem: checker.f and xref.f account for 13.9 s (7.9%) of the forced-tier load while the entire tier-0 load, checking included, is 0.87 s; something in the pipeline re-runs checker or xref work per definition or per call site (CHECKER-CALLS replay is the suspect). Acceptance: the callers into checker.f/xref.f from src/compiler are named with counts; whatever is recomputed per definition is computed once or read from the record; the 13.9 s falls to the tier-0 share; controlled pair. Files: to be named by the attribution. Verify: sampling or counters over the forced-tier load. Depends: none. Ownership: rowan. Claim: unassigned
