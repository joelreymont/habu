---
title: Add a unix datagram package
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T17:25:29.298279+01:00"
---

Problem: Omabox's omaboxd test reads NOTIFY_SOCKET by copying src/lib/systemd-test.f:49-141's private socket, bind and recv bindings; the library has IPv4 datagrams (src/lib/net/udp4.f) but none on a unix socket path. Acceptance: package UDP (src/lib/net/udp.f, beside udp4.f; Joel 2026-11-17) binds a unix datagram socket at a path (abstract names included), receives one datagram within a timeout as UDP4:RECEIVE does, and sends one datagram to a path, and closes it; src/lib/systemd.f (SYSTEMD:NOTIFY's private socket and sendto, :35-80) and systemd-test.f:49-141 use it in place of their private copies. Depends: none. Claim: unassigned.
