---
title: Copy the seal cells into worker task regions
status: open
priority: 3
issue-type: task
created-at: "2026-10-03T20:58:01.309700+03:00"
---

lib/policy.f's seal lives in two cells of the sealing task's DATA (POLICY-NDICT-CELL $4898, POLICY-BITS-OFF $48A0..$4CA0). lib/task.f TASK-REGION-INIT copies only named cells into a new task's region, so a worker task reads unsealed: its prims do not refuse. No design route reaches this today, because a design cannot name the prims and workers read no design source unless an admitted word hands them caller data. Add the two cells to TASK-REGION-INIT's copied rows so the seal is process-wide, then restate docs/stdlib.md's storage row. Found by the Fable review seal-rev (F1) and Astra while landing habu-load-authored-src-4ef714a3.
