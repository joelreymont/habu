---
title: Remove string-compare fallbacks in keyword handling
status: closed
priority: 2
issue-type: task
assignee: ""
created-at: "2025-12-10T10:47:59.772909+02:00"
closed-at: "2025-12-10T11:14:22.102104+02:00"
close-reason: ""
---

Replace get-keyword-name string comparison with pointer comparison after boundary normalization. String fallbacks hide mismatches.
