---
title: Find why the idle daemon grows exit rows
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T13:38:50.543593+02:00"
---

Problem: the idle Omabox daemon registers more exit rows than the first chunk holds, so src/lib/task.f EXIT-GROW maps another EXIT-CHUNK-BYTES (16 KiB) chunk; which tasks owe those rows is not known. Acceptance: the cause is found, and the idle daemon's exit rows are only those its live tasks owe. Depends: none. Claim: unassigned.
