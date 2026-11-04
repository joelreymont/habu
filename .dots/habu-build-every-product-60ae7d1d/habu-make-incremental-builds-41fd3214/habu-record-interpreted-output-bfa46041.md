---
title: Record interpreted output while a parent moves output off the tee
status: closed
priority: 2
issue-type: task
created-at: "2026-10-24T04:43:55.288761+03:00"
closed-at: "2026-10-31T20:52:55.876806+02:00"
close-reason: "Obsolete after lane 11: the cache keeps no output records; the re-run top level writes output live."
---

Workspace: carl-tee (sxkulsnz). When a cold parent has routed output to another device, an interpreted ." in a child is neither recorded nor judged, so a hit loses that output (the same class as Astra tee F2). Evidence: re-review of smormxxm, 'Not covered'.
