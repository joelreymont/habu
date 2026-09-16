---
title: Add TCP sockets to lib/net
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T13:16:06.962213+03:00"
---

Problem: lib/net has only udp4.f, so nothing in Habu can accept or open a TCP connection, which every server, HTTP client and remote REPL needs. SwiftForth's lib/options/tcp.f exposes /LISTEN /ACCEPT /CONNECT TCP-READ TCP-READX TCP-WRITE over a current SOCKET:; VFX exposes TCPConnect, readsock, writesock, pollsock and listen through its device open (docs/socket-models.md). Acceptance: lib/net/tcp4.f, package TCP4, typed like UDP4 (address, port, distinct listener and connection socket types, result ADTs carrying errno): BIND, LISTEN with a backlog, ACCEPT (blocking, returning the connection and the peer endpoint), CONNECT, READ (partial), READ-EXACT, WRITE (all bytes or failed), SHUTDOWN, CLOSE and a non-blocking readable check; Linux aarch64 glibc like udp4.f, other systems rejected before opening. Files: lib/net/tcp4.f, lib/net/tcp4-test.f, docs/tcp4.md mirroring docs/udp4.md. Verify: a loopback echo test with a listener task and a client in one process; connect refused and read after close return failed errno; bin/hb --load test/run.f green. Depends: none. Ownership: lib/net/tcp4.f, its test and doc. Claim: unassigned.
