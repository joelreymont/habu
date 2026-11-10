---
title: "Census: every cell a capture hook clears, plain or scratch"
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T12:26:39.683580+02:00"
---

Problem: a program build scrubs process state out of what it writes by running hooks (IMAGE-LIFECYCLE:REGISTER, run by PREPARE from app-end.f and end.f). A hook exists only in the process whose top level registered it, while a file object carries the plain cells that hook would clear, so a build that links such a file from its object writes the builder's state into the program, or throws. DECISIONS 2026-11-09 'values a linked object replays' settles the rule: each owner keeps its cells free of process state as scratch cells, linked as zero. Fixed so far: the hook count and five flags (c1efdd1a). 27 REGISTER call sites in 23 files remain unchecked (rg IMAGE-LIFECYCLE:REGISTER under src and tools); build-out.f and engine-id.f are habu-build-out-and-322dc2f5, task.f is habu-task-cells-a-5a896f0c. Acceptance: for every call site, the hook, each cell it writes, whether that cell is plain, scratch, a PTR cell, a TASK:+USER slot or a DYNAMIC-BUFFER cell, and whether a file's top level can reach the registration; then one fix dot per group of files whose cells are plain. Files: read-only. Verify: the table in the report, each row a path:line. Depends: none. Ownership: none (read-only). Claim: agent=jerry workspace=.jj-ws.noindex/jerry-hooks (read-only census).
