---
title: Reject a top-level 2drop of a str in the check
status: open
priority: 3
issue-type: task
created-at: "2026-11-25T17:13:54.001545+01:00"
---

Problem: at top level `s" x" 2drop` is E-LAYOUT-VALUE on the load and passes check.f --verify-only. Acceptance: the check reports the load's rejection at the same place; a test fails without it. Depends: none. Claim: unassigned.
