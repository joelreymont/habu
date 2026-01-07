---
title: Integrate TCO into SBCL compilation pipeline
status: closed
priority: 1
issue-type: task
assignee: ""
created-at: "2025-12-05T09:28:12.195823+02:00"
closed-at: "2025-12-06T21:26:31.40536+02:00"
close-reason: ""
---

TCO (tail call optimization) exists in optimize.lisp but needs to be properly integrated into the SBCL bootstrap compiler path. Required for cross-platform bootstrap - recursive functions without TCO will stack overflow on platforms with smaller default stacks.
