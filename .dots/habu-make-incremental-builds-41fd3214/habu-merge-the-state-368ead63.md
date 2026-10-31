---
title: "Merge the state e2e's two cold-build helpers"
status: active
priority: 2
issue-type: task
created-at: "2026-10-25T06:59:21.903245+01:00"
---

test/native-file-state-e2e.f COLD-BUILD (:113) and FRESH-BUILD (:398) both build app.f with an empty cache of their own; one helper should serve both callers. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-stm (brief carl-dots/.out/carl-file/brief-stm.md).
