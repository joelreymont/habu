---
title: "Delete AIO's loop words"
status: open
priority: 2
issue-type: task
created-at: "2026-11-27T13:47:56.514031+01:00"
---

Problem: once each task waits on its own queue (the AIO line in .jj-ws.noindex/jerry-aq), AIO:START, AIO:STOP, AIO:RUNNING?, AIO:READ, AIO:AWAIT-XFER and type AIO:xfer serve nothing: no loop is left to start, and READ and AWAIT-XFER have one in-tree caller (src/lib/net/http-response.f). Joel approved deleting START and STOP on 2026-11-27, though Tender, Zen, Maki, Niwa, SAP, Kiba and omabox call them. Acceptance: the words and type are gone; http.f, curl.f and the comments at pty.f, serial.f and process-pty-io.f follow; the install notice names them and each caller's owner is told before the install; dots habu-grow-the-linux-fd2c8749 and habu-close-aio-s-197bae7c, about the loop's ring and STOP, are closed with it. Depends: the AIO per-task queue line landing. Claim: unassigned.
