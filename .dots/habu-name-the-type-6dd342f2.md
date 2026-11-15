---
title: "Name the type in an ambiguous stored row's help"
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T22:31:34.807960+01:00"
---

Problem: src/core/checker.f SGBAD-HELP's SGBAD-AMBIG? branch hands SGBAD-TOKEN to SGBAD-AMBIG-HELP; SGBAD-TOKEN answers the declared word's name when the rejected token is the whole row, so such a help would name the word in place of the ambiguous type, the defect the other branch no longer has. Acceptance: if a stored row whose whole text is an ambiguous type name reaches the branch, its help names that type and a test pins it; if none does, the branch takes the token as the other branch does. Depends: none. Claim: unassigned.
