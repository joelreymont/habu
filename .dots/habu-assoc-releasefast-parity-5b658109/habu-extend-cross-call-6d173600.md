---
title: Extend cross-call BL patch 64-bit
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-02-23T09:29:14.786354+01:00\\\"\""
closed-at: "2026-02-23T09:37:48.684727+01:00"
close-reason: Extend patchCrossCallsToBL for 64-bit targets and cover it
---

src/jit/backend.zig patchCrossCallsToBL: support MOVZ+MOVK+MOVK+MOVK target materialization and cover with backend unit test so helper BL patching works when target high16 bits are non-zero.
