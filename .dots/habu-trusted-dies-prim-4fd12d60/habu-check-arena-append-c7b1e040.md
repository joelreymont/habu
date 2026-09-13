---
title: Check arena append ranges without overflowing the endpoint
status: open
priority: 1
issue-type: task
created-at: "2026-09-13T14:51:13.068869+03:00"
---

Cedar source review of 5226a994, 2026-09-13: src/compiler/ir/arena.f:493 APPEND-SPAN validates from+k with signed addition before copying. Large positive from plus an allowed k can wrap negative and pass the upper bound. Validate from against source count first, then k against count-from before any growth or copy; retain zero-length endpoint behavior and unchanged destination on refusal. This is source-confirmed; no invalid-memory runtime probe required. Unassigned.
