---
title: Keep the HTTP listener alive at the AIO ceiling
status: active
priority: 2
issue-type: task
created-at: "2026-11-17T08:29:21.773509+01:00"
---

Problem: src/lib/net/http.f ARM-ACCEPT (:793-800) re-arms every ACCEPT-WAIT-MS and, unlike ARM-ROW, does not catch a refused poll; at Linux's AIO record ceiling (E-AIO-FULL) or Darwin's poll bound the listener ends and the server stops accepting while it runs (ARM-WAKE and ACCEPT-PAUSE likewise). ws-test.f FAULTING meets it: the websocket suite hangs 4 of 136 runs on master (/tmp/jerry-ws-hang). Acceptance: the listener's polls stand until an event, a stop wakes it, a refused re-arm pauses and retries; an http-test.f case red before and green after; ws-test.f 40 of 40 runs pass. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-http.
