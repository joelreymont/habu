---
title: "Drop TCP4's and SERIAL's remaining runtime checks"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-17T09:55:08.394618+01:00"
closed-at: "2026-11-22T14:42:57.135332+01:00"
---

Problem: these still check at run time (paths in src/lib/net/tcp4.f and src/lib/serial.f): WAIT-READY's timeout range (tcp4.f:358) and DEADLINE (serial.f:189); the 'cancelled' arms AIO cannot answer here (tcp4.f:363, serial.f:216; MATCH needs the arm, so AIO's outcome type must change); SEND-BY's 0= (tcp4.f:432); OWN-SOCKET's and NONBLOCK's fcntl results (tcp4.f:269, :277); ENDPOINT!'s address and port (tcp4.f:201-202), which ADDRESS and PORT already check; OPEN8N1's baud (serial.f:237), which BAUD already checks; LISTEN's backlog (tcp4.f:548), which the kernel clamps; READ, TRY-READ and READ-EXACT's 1..MAX-TRANSFER (tcp4.f:588,599,607), where a zero READ throws in TCP4 but answers transferred 0 in SERIAL; SEND-SPAN's 1 < (tcp4.f:440) and WRITE's MAX-TRANSFER cap (tcp4.f:660); PATH-OPEN's empty path (serial.f:132); BYTES's range (serial.f:228), which protects nothing now; LENGTH's five unreachable MATCH arms (tcp4.f:168-177, serial.f:66-75). Acceptance: each is gone, a type or the kernel's own answer covers it, or it sits where a raw number first becomes a typed value; TCP4 and SERIAL answer a zero-byte request alike. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-tsc.
