---
title: Fix FASL linker (Invalid FASL magic)
status: closed
priority: 1
issue-type: bug
assignee: ""
created-at: "2025-12-07T08:10:48.991158+02:00"
closed-at: "2025-12-25 07:21:12"
close-reason: "Obsolete: Zig rewrite"
---

6 FASL linker tests fail with "Invalid FASL magic". The linker isn't reading/writing FASL files correctly.
