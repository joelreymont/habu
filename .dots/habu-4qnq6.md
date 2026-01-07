---
title: Add keyword normalization at h0-eval-builtin boundary
status: closed
priority: 1
issue-type: task
assignee: ""
created-at: "2025-12-10T10:47:58.96186+02:00"
closed-at: "2025-12-10T11:05:24.126566+02:00"
close-reason: ""
---

Convert SBCL keywords to native habu keywords at the dispatch boundary. Cache converted keywords to enable pointer comparison instead of string comparison.
