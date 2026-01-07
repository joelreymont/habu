---
title: Implement unwind-protect
status: closed
priority: 2
issue-type: feature
assignee: ""
created-at: "2025-12-04T22:11:12.102166+02:00"
closed-at: "2025-12-25 07:22:05"
close-reason: "Obsolete: Zig rewrite"
---

Guarantees cleanup code runs even on non-local exit. (unwind-protect protected-form cleanup-forms...). Essential for resource management (files, locks).
