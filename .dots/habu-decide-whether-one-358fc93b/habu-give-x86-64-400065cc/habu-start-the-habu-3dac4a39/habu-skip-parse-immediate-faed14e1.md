---
title: Skip parse-immediate operands in T0
status: closed
priority: 2
issue-type: task
created-at: "2026-10-28T13:35:40.400550+02:00"
closed-at: "2026-11-02T11:34:09.191686+02:00"
close-reason: "Superseded: src/habu/t0.f IMM-TOKEN (~1546) skips CHECKER-OWNER:PARSE-IMM? words and refuses others with E-UNMODELED-IMMEDIATE; E-T0-IMMEDIATE is gone (7d55e756b2)."
---

Design (fable-t0-l1-design.md Immediates): T0 skips parse-neutral user immediates (NEUTRAL-PARSE-IMM?, checker.f:22113, e.g. GRID:) with their operands and refuses other immediates by name (E-T0-IMMEDIATE). Not done in commit 1. Workspace: jerry-t0 stack.
