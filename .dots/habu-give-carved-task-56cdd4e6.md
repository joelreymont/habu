---
title: Give carved task-local rows a record type
status: open
priority: 2
issue-type: task
created-at: "2026-11-22T12:55:57.007232+01:00"
---

Problem: modules carve a TASK:+USERS row into cells or slots by offsets, CELL-VIEW or ptr-field, though TASK:+USER answers a typed ( -- ^t ) accessor: src/lib/zlib.f:53-58 (SCRATCH: z_stream and a probe byte by ptr-field and offsets), src/lib/task.f:1672-1678 (SLEEP-SPECS, two timespecs by CELL-VIEW), src/lib/c2-memory.f:30-56 (OWNER-REGION: five header cells and FRAME-CAP frames by offsets and casts). c2-memory's frames are also written by the c2-init-stow and c2-records-stow primitives at fixed offsets (src/habu/habu1.f:3300-3305, :3331-3334), whose rows take the frame as ^n (src/habu/prims.f:283-290); PE-PTR-A-STORED gives a row a stored pointee, so those rows can take the frame record. Acceptance: each such row is a STRUCTURE held through TASK:+USER(S), or one row per role (a byte row stays only where an OS call fills raw bytes and says so); no offset sum, index or CELL-VIEW carves a task-local row in src/lib; the modules' tests pass. Depends: none. Claim: unassigned.
