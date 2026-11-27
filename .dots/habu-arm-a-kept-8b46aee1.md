---
title: Arm a kept-alive connection once per request
status: open
priority: 2
issue-type: task
created-at: "2026-11-27T18:57:09.727167+01:00"
---

Problem: on spark a kept-alive request costs the listener 0.25-0.45 EPOLL_CTL_MOD on the connection's descriptor (strace of /tmp/jerry-p1/ctxsw/drive.py over srv.f): the listener adopts a POLL-INTO record from its inbox while the worker's arm is still unconfirmed (owner pushed), wins ADOPT-OWNER's CAS and arms the descriptor itself (src/lib/aio.f ADOPT-ONE), so the descriptor is armed twice. Acceptance: a kept-alive GET loop makes one epoll_ctl per request on the connection, from the worker, and still two thread switches per request. Depends: none. Claim: unassigned.
