---
title: Save native binaries without embedded heap
status: closed
priority: 1
issue-type: task
assignee: ""
created-at: "2025-12-04T10:33:24.777909+02:00"
closed-at: "2025-12-04T10:50:56.884766+02:00"
close-reason: ""
---

Instead of embedding heap data in Mach-O binary, use mmap to allocate heap at runtime. This allows smaller binaries and dynamic heap sizing.
