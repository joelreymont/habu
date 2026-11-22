---
title: Start WS from a config structure
status: active
priority: 2
issue-type: task
created-at: "2026-11-23T09:27:32.558856+01:00"
---

Problem: Joel: all server starts take a config structure. WS:START takes a bare socket count and WS's ping, pong and stall bounds are constants, so ws-test waits them out. Acceptance: WS:START takes a WS:config (sockets, ping, pong, stall) with WS:DEFAULTS; ws-test runs with short bounds; websocket passes. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-p4ws
