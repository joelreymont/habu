---
title: AIO waits keep their operation in flight through a halt
status: open
priority: 2
issue-type: task
created-at: "2026-11-02T17:09:00.870124+02:00"
---

Problem: src/lib/aio.f WAIT-DONE (:1114-1119) and AWAIT-GROUP (:1252-1259) are TASK:STOP loops that never read TASK:HALTED?, which task.f (:2273-2276) says a STOP loop reads after every STOP so the wait gives its resources back before the PAUSE that ends the task. A halt therefore unwinds with the awaited record still SUBMITTED until SCRUB, an AT-EXIT, cancels it after every cleanup has run. On Darwin (src/lib/aio-macos.f) the poll thread polls by descriptor number and, for READ, WRITE, ACCEPT and CONNECT records, calls read, write, accept, getsockopt and fcntl on that number (MAC-SERVICE :320-333, MAC-TAKE-FLAGS :193-199). A cleanup that closes the descriptor (the natural finally) lets another task's new socket take the number, and the dead record then reads, writes or changes flags on that socket. Linux is not affected: io_uring holds the file. PG CONNECT's halt cleanup (dot 7e09e8e4) waits on a POLL record, which only reports revents, so it is not exposed. Acceptance: after TASK:STOP, when TASK:HALTED?, WAIT-DONE cancels its record and AWAIT-GROUP its group's in-flight records (HOST-CANCEL, state left SUBMITTED so the record still completes as cancelled and SCRUB keeps its job); a halted task whose cleanup closes a socket while a READ is pending on it, with a second task opening a socket that reuses the number, sees no byte taken from the second socket. Files: src/lib/aio.f, src/lib/aio-test.f or the halt rows beside it. Verify: the aio suite rows, test/db/pg-cluster.f. Depends: none. Ownership: src/lib/aio.f. Claim: unassigned.
