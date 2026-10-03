---
title: Read WebSocket pings in bulk
status: open
priority: 3
issue-type: task
created-at: "2026-10-03T16:55:36.894994+03:00"
---

lib/net/ws.f reads each incoming frame in small pieces (READABLE? then READ, about six syscalls plus a claim, a send and a PAUSE per pong; ws.f ~510-520, ~590-600). A client flooding pings is answered at 115-250 pongs/s under load, so the websocket suite's pong-fill cases take 11-72 s and grow with load. Reading the stream in bulk would cut that several-fold for every user. Found by Fable ws-rev13 while landing habu-add-rfc-6455-8aa83bda.
