---
title: Run the network and callback paths on Linux
status: open
priority: 3
issue-type: task
created-at: "2026-10-03T21:36:59.836450+03:00"
---

The features that landed on 2026-10-03 run only on macOS so far. Their Linux branches are reasoned, not run: TCP4 ACCEPT through accept4 with SOCK_NONBLOCK; the Linux values of O_NONBLOCK ($800), F_SETFL 4, POLLIN 1, POLLOUT 4 and EAGAIN 11; TCP4:UNSENT through SIOCOUTQ ($5411), counting a FIN sent by SHUTDOWN until it is acknowledged; TCP4:UNREAD through FIONREAD ($541B); the WebSocket pong fill and PONG-FILLED's progress bound (7,534 pongs on macOS; whether SO_RCVBUF set before connect shrinks the fill on Linux, as it does not on macOS); the HTTP listen backlog under a burst. Run lib/net/tcp4-test.f, http-test.f, ws-frame-test.f, ws-test.f, lib/ffi-callback-test.f, lib/task-test.f, lib/policy-test.f and the full registry on a linux-aarch64 engine (and on linux-x86-64 once its FFI port lands), and fix what differs. Habu builds on linux/arm64. Joel chose on 2026-10-03 to defer this until it is needed, ahead of Maki's Linux GPU hosts.
