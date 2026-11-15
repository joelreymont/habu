---
title: "Delete DECL-EVENT's arity check and family slot"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T17:19:45.347346+01:00"
closed-at: "2026-11-16T04:07:36.438449+01:00"
---

Problem: src/core/decl-event.f ARITY repeats DECL-HEADER's parameter count check, and the event row's FAM slot compares the one open family with itself. Acceptance: both are gone, with their calls and comparisons. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
