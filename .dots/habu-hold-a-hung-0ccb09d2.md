---
title: "Hold a hung-up socket's read without spinning"
status: open
priority: 2
issue-type: task
created-at: "2026-11-27T09:56:38.102665+01:00"
---

Problem: src/lib/net/ws.f READY (:1307-1313). With the slot's read held (CALLS calls out) and a write-only poll armed, a hang-up or error reported without POLLOUT leaves READY at once, SYNC re-arms the same poll, and the kernel reports the hang-up again at once, so WS's task spins until a worker's call ends. If a call's end arms the read between the arm and the event, READY calls INPUT, whose read lands at IN's start over bytes still unparsed (IN-AT, IN-U), losing them. Acceptance: a hang-up or error while the read is held or bytes are unparsed neither busy-waits WS's task nor drops a byte read; the slot's course states which of its states takes the hang-up. Depends: none.
