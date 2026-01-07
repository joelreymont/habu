---
title: Unify wrapper code paths - remove mmap wrapper, use only __DATA segment approach
status: closed
priority: 1
issue-type: task
assignee: ""
created-at: "2025-12-08T08:38:52.804683+02:00"
closed-at: "2025-12-09T09:26:56.89464+02:00"
close-reason: ""
---

Currently have two wrapper approaches: 1) mmap-at-runtime wrapper (wrap-bytecode-with-mmap-heap) and 2) __DATA segment wrapper (wrap-bytecode-with-heap-for-imports). This causes confusion, code duplication, and wrapper-size inconsistencies. Unify to single __DATA approach which is cleaner and faster.
