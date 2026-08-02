---
title: Add bounded socket calls
status: open
priority: 1
issue-type: task
created-at: "2026-07-29T22:07:09.339177+02:00"
blocks:
---

Why: the native image lacks the libc calls required by the product server and loopback client. Interface: package SOCK-OS owns exactly nine named TRUSTED declarations: SOCKET-CALL maps socket(int,int,int)->int; SETSOCKOPT-CALL maps setsockopt(int,int,int,const void*,socklen_t)->int; GETSOCKOPT-CALL maps getsockopt(int,int,int,void*,socklen_t*)->int; BIND-CALL maps bind(int,const sockaddr*,socklen_t)->int; LISTEN-CALL maps listen(int,int)->int; ACCEPT4-CALL maps accept4(int,sockaddr*,socklen_t*,int)->int; CONNECT-CALL maps connect(int,const sockaddr*,socklen_t)->int; GETSOCKNAME-CALL maps getsockname(int,sockaddr*,socklen_t*)->int; ERRNO-LOCATION-CALL maps __errno_location(void)->int*. This leaf adds only those atomic boundaries and exact checker effects; the checked owner leaf composes them. Reuse existing read, write, fcntl, poll, and close declarations. Owner: nine socket libc boundaries only. Production red: the native image cannot create, accept, or complete a nonblocking client connection. Acceptance: each declaration calls its real symbol with exact argument and return widths; wrong spans and effects reject through the production checker; each boundary carries source-local rationale, this dot as retirement owner, and a focused production-path test. Forbidden: checked wrapper, descriptor owner, DNS, IPv6, Unix socket, TLS, thread, task runtime, signal handler, generic syscall layer, dynamic symbol lookup, retry, ABI version, or compatibility declaration. Smallest owning check: bin/hb --load lib/socket-os-boundary-test.f. Claim: unassigned.
