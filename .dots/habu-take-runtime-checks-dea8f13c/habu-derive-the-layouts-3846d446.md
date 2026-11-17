---
title: Derive the layouts still written by hand
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T16:46:36.414820+01:00"
---

Problem: record layouts restated as hand-written offsets that nothing checks against a declaration: the XML frame, attribute and namespace records (src/lib/xml/state.f ATTR-CELLS, ATTR-HASH, NS-CELLS and their neighbours), src/lib/xml/source.f SRC-HEADER-CELLS, and src/lib/task.f's facility, semaphore-guard and timespec records (TASK-FACILITY-*, TASK-SEM-GUARD-OFF, TASK-TIMESPEC-*, TASK-SPEC-*). Acceptance: each layout is declared once and its offsets derive from the declaration, as TASK-ABI's TCB offsets do. Depends: none. Claim: unassigned.
