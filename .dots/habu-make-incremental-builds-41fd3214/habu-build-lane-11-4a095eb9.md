---
title: "Build lane 11's engine from a host that exists"
status: active
priority: 2
issue-type: task
created-at: "2026-10-31T09:59:38.360781+02:00"
---

On the rebased stack, master engines die -8597 after util.f running the stack's hb-build; lane engines lack spawn-fds and refuse master's libraries. Find the cause; fix the stack or add a staged-host route. Brief carl-dots/.out/carl-file/brief-l11-host.md. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-l11h.
