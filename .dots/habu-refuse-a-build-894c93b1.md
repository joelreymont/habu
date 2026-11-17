---
title: Refuse a build that keeps an AIO ticket
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T10:26:03.401573+01:00"
---

Problem: a ticket or xfer kept in a variable across a build names a record in chunks the build leaves out (src/lib/aio.f SCRATCH-BUFFER and SCRATCH-VARIABLE, :232 and :240). In the built program AWAIT on such a ticket walks the record chunks; an index of RECS-FIRST (64) or more reaches a chunk link of 0 and CHUNK-AFTER (src/lib/aio.f:370) dies with E-AIO-STATE, exit 67; a lower index is accepted and awaits whichever record now has that index (test/fixtures/file-objects/kept-aio.f). CHUNK-AFTER is a runtime check of a value no saved program can use. Acceptance: building a program whose saved image keeps an AIO ticket or xfer is rejected with a message naming the variable; CHUNK-AFTER's guard is deleted; kept-aio.f's case expects the build rejection. Depends: none. Claim: unassigned.
