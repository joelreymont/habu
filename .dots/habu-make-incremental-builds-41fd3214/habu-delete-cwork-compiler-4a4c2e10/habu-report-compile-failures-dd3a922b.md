---
title: Report compile failures inside the context
status: active
priority: 1
issue-type: task
created-at: "2026-10-26T13:52:38.570269+02:00"
---

CWORK lane 0: compiler.f moves REPORT-FAILURE / REPORT-UNDEFINED-TICK into BODY after the catch, before RETIRE-BODY, so RUN no longer reads context data after WITH-CONTEXT returns. Brief ~/.cache/tmp/carl-file/brief-cw0.md. Workspace: .jj-ws.noindex/carl-cw0. Claim: agent=carl worker.
