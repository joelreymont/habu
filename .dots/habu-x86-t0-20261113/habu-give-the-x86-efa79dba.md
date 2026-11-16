---
title: Give the x86 kernel an evaluate entry
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T10:48:16.844111+01:00"
---

Problem: x86 evaluate is provided Habu code with no EVALREC entry, so link-data.f branches on X64? at :174, :185, :462 and writes EVALREC-CELL 0. Acceptance: x86 has arm64's evaluate entry and the X64? branches are gone. Depends: none. Claim: unassigned.
