---
title: Mark boundary functions notinline
status: closed
priority: 2
issue-type: task
assignee: ""
created-at: "2025-12-10T10:48:00.201171+02:00"
closed-at: "2025-12-10T10:49:47.295326+02:00"
close-reason: ""
---

Add (declaim (notinline ...)) to all functions that cross the SBCL/native boundary to prevent stale inlined code.
