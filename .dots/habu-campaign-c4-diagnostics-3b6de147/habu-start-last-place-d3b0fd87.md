---
title: "Start LAST-PLACE's byte at -1"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-16T03:01:48.110765+01:00"
closed-at: "2026-11-16T04:07:34.014123+01:00"
---

Problem: src/core/rejection.f LAST-PLACE says its byte is -1 for a record with no place, but variable LAST-AT starts at 0, so before any record it answers 0. Acceptance: LAST-PLACE answers -1 before any record. Depends: none. Claim: agent=W166 workspace=.jj-ws.noindex/jerry-w166.
