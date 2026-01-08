---
title: Fix VM I/O error masking
status: open
priority: 2
issue-type: task
created-at: "2026-01-08T12:26:01.251430+02:00"
---

File: src/interp/vm.zig:1352-1358 - Change print/princ operations to propagate errors instead of 'catch return error.Halt'. Need to ensure VM error type includes I/O errors.
