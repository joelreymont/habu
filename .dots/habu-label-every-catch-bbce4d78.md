---
title: Label every catch-stale section
status: open
priority: 2
issue-type: task
created-at: "2026-11-23T18:04:10.712949+01:00"
---

Problem: test/catch-stale-suite.f's sections REPRODUCERS through PROOF and EDGES through OPEN set no T-LABEL, so a failure in one reports under the label last set (an O9 failure printed as PP2's 'normal output loss stays with its occurrence'). Acceptance: each section sets its own T-LABEL first; a failing case reports under its section's label. Depends: none. Claim: unassigned (its commit is on the parked stack lmrmpkys..xvroprym above tqsnwlwz, workspace .jj-ws.noindex/carl-infname).
