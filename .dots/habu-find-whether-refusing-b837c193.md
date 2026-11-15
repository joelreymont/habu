---
title: Find whether refusing a loop that refreshes a caught cell is sound
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T13:29:30.050833+02:00"
---

Problem: a loop body that replaces a carried caught cell with a fresh value (lineage 0 against the entry's -1) is refused; hunt programs K3, F1, N6, S1 (lane carl-loopp, change 92ee5249, 'Let a loop carry a cell that lost its catch proof'). Is each refusal needed for soundness (a later read could see a stale value) or an over-refusal of the carried-cell rule (CP-CARRIES?, GROUP-META=/ROW-META=)? Acceptance: a hunt report per program: refusal sound with the stale read it prevents, or over-refusal with the rule's cause as path:line; an over-refusal becomes its own fix. Files: src/core/checker.f loop carry rule. Verify: the four programs on the carl-loopp engine, outcome table. Depends: carl-loopp landed. Ownership: none (read-only hunt). Claim: unassigned.
