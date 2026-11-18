---
title: "Add HTTP:CURRENT-ID$"
status: active
priority: 2
issue-type: task
created-at: "2026-11-19T12:06:02.965379+01:00"
---

Goal: a handler names the request it serves by the id HTTP:LOG prints (mark's request; Joel approved the name 2026-11-19); the router's table grows past 32 routes so the suite's added route fits. Acceptance: HTTP:CURRENT-ID$ true with the id inside a request, false elsewhere, reviewed, http suite passes, installed on spark and neubau. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-reqid
