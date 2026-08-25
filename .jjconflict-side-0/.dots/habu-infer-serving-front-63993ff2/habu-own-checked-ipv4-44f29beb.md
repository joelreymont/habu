---
title: Own checked IPv4 sockets
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T01:12:14.086652+02:00"
blocks:
  - habu-add-bounded-socket-57975fa4
---

Why: raw socket boundaries are not safe descriptor owners. Interface: package SOCK-OS composes retained read, write, fcntl, poll, and close with the nine boundaries for numeric IPv4 only. OPEN-LISTENER returns opened(listener,bound-port) or refused(sock-error). CONNECT sets close-on-exec and nonblocking before connect, then returns connected(conn), connecting(conn), or refused(sock-error); FINISH-CONNECT uses GETSOCKOPT SO_ERROR after writable readiness and returns connected(conn), connecting(conn), or refused(sock-error). ACCEPT returns accepted(listener,conn), would-block(listener), or refused(listener,sock-error). READ returns progress(conn,count), would-block(conn), eof(conn), or refused(conn,sock-error); WRITE returns progress(conn,count), would-block(conn), or refused(conn,sock-error). CLOSE always consumes the descriptor and returns closed or close-failed(sock-error); a failed close never returns a reusable descriptor and is never retried. Every address and span validates before libc and errno is captured immediately. Owner: checked IPv4 socket construction and descriptor lifetime only. Production red: the server and loopback client cannot safely create descriptors. Acceptance: bind port zero, GETSOCKNAME, immediate and pending CONNECT, SO_ERROR refusal, ACCEPT, partial I/O, would-block, bad address, short socklen, errno, acquisition rollback, and consumed close failure follow exact ownership; two listeners and clients coexist. Forbidden: raw TRUSTED declaration, DNS, IPv6, Unix sockets, TLS, thread, retry loop, generic syscall layer, ABI version, or compatibility wrapper. Smallest owning check: bin/hb --load lib/socket-test.f.
