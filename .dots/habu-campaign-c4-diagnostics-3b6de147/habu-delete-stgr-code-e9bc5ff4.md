---
title: Delete STGR-CODE, a copy of CODE-SPAN
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T20:50:18.982109+01:00"
closed-at: "2026-11-16T04:07:36.406325+01:00"
---

Problem: src/core/checker.f STGR-CODE (:13673) writes a code span as CODE-SPAN (:11307) does. Acceptance: its callers call CODE-SPAN and STGR-CODE is gone. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
