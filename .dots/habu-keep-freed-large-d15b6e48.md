---
title: Keep freed large heap blocks off memory
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T13:38:50.531049+02:00"
---

Problem: HEAP:ALLOCATOR is the system allocator. In glibc, freeing a block it mapped (128 KiB or more) raises its dynamic mmap threshold (mallopt(3)), so later large blocks come from the brk heap and can stay resident after they are freed. Acceptance: after a sync pass, the idle Omabox daemon's heap holds no freed large block, measured in /proc/PID/smaps. Depends: none. Claim: unassigned.
