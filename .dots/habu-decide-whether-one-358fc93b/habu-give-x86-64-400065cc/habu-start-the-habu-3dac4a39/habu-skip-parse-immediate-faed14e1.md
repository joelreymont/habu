---
title: Skip parse-immediate operands in T0
status: open
priority: 2
issue-type: task
created-at: "2026-10-28T13:35:40.400550+02:00"
---

Design (fable-t0-l1-design.md Immediates): T0 skips parse-neutral user immediates (NEUTRAL-PARSE-IMM?, checker.f:22113, e.g. GRID:) with their operands and refuses other immediates by name (E-T0-IMMEDIATE). Not done in commit 1. Workspace: jerry-t0 stack.
