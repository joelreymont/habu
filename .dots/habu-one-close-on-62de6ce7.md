---
title: "One close-on-exec pipe word, FD:PIPE"
status: active
priority: 2
issue-type: task
created-at: "2026-11-05T11:09:05.130815+02:00"
---

Problem: the HTTP server's new wake pipe (lane carl-http1, src/lib/net/http.f WAKE-OPEN) and the WebSocket server's (src/lib/net/ws.f:1182 PIPE-OPEN) are not close-on-exec, so every program a handler starts inherits both ends; process.f:303-315 (PIPE-PAIR, PIPE-PAIR-CLOEXEC-HELD, PIPE-PAIR-CLOEXEC, a global outside any package) and aio-macos.f:270-280 (MAC-PIPE-HELD) each mark theirs under FD-LOCK:WITH in their own copy. Joel 2026-11-05 chose 'Approve FD:PIPE (Recommended)': 'One pipe word in FD; HTTP, WebSocket, AIO and process code use it.' Acceptance: public FD:PIPE ( -- fd fd ) in src/lib/fd.f, read end first, made under FD-LOCK:WITH with FD_CLOEXEC on both ends, a failure E-FS-IO as fd.f's other words; process.f's three pipe words go and their callers (signal.f:210, process-fork.f:154, process-command-test.f:235,460,469-470, the process-pty-io.f:149 comment) use FD:PIPE; aio-macos.f's MAC-PIPE uses it and keeps only its nonblocking fcntl (the lock does not nest: drop MAC-PIPE's own WITH); ws.f PIPE-OPEN uses it and E-WS-WAKE goes if nothing else throws it. Files: src/lib/fd.f, src/lib/process.f, src/lib/signal.f, src/lib/process-fork.f, src/lib/process-pty-io.f, src/lib/process-command-test.f, src/lib/aio-macos.f, src/lib/net/ws.f, src/lib/errors.f. Verify: process-command-test.f, spawn-test.f, signal tests, ws-test.f, aio tests on macOS; Linux on ssh spark for the process and signal rows. Depends: none. Ownership: carl. Claim: agent=worker workspace=.jj-ws.noindex/carl-fdpipe.
