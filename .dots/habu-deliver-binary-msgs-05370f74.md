---
title: "Deliver binary messages to a view's code"
status: active
priority: 2
issue-type: task
created-at: "2026-11-23T19:19:15.476092+01:00"
---

Problem: Maki's view gets file bytes (artifact, ranges; maki src/tool.f:59-64, :1147-1159) as base64 in the JSON answer's _meta; Maki's no-text form (Joel approved 2026-11-23) sends them as WebSocket binary messages after the answer, and src/lib/browser/socket.js carries text frames only. Acceptance: the view's code receives each binary message tied to its answer, through socket.js's machine. Depends: none. Claim: agent=carl-wsbin-plan (design) workspace=none.
