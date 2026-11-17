---
title: Refuse a build that keeps an AIO ticket
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T10:26:03.401573+01:00"
---

Problem: a ticket or xfer kept in a variable across a build names a record in chunks the build leaves out (src/lib/aio.f SCRATCH-BUFFER and SCRATCH-VARIABLE, :232 and :240). In the built program AWAIT on such a ticket walks the record chunks; an index of RECS-FIRST (64) or more reaches a chunk link of 0 and CHUNK-AFTER (src/lib/aio.f:370) dies with E-AIO-STATE, exit 67; a lower index is accepted and awaits whichever record now has that index (test/fixtures/file-objects/kept-aio.f). CHUNK-AFTER is a runtime check of a value no saved program can use. Acceptance: a NEWTYPE written `NEWTYPE ticket <base> SCRATCH-ONLY` holds values valid only in the process that made them, and AIO's ticket and xfer are so declared. A static typed definer (TYPED-VARIABLE, TYPED-BUFFER and their kin) whose stored type holds such a family is rejected, E-BAD-STORAGE, naming the variable, with help to declare it scratch, unless the `SCRATCH-ONLY` clause follows its type (`TYPED-VARIABLE V curl-handle SCRATCH-ONLY`), which keeps the storage out of the saved image. Definers whose storage is never saved (DYNAMIC-BUFFER, ARENA, HEAP, task USER) reject the clause; `scratch-only` is rejected as a family name; every type reader (storage definers, NEWTYPE's base, tools/check.f) stops at the keyword. src/lib/curl.f :733, :747, :748 and src/lib/ws.f :251 use the clause. CHUNK-AFTER's guard is deleted; kept-aio.f becomes a rejected-build case; kept-main.f's dead block (:12-16) goes. A warm build equals a cold one. Joel 2026-11-17 chose the one clause SCRATCH-ONLY on NEWTYPE, TYPED-VARIABLE and TYPED-BUFFER. Depends: none. Claim: unassigned.
