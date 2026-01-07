---
title: Link-time undefined function verification
status: closed
priority: 1
issue-type: feature
assignee: ""
created-at: "2025-12-05T13:37:44.996927+02:00"
closed-at: "2025-12-25 07:21:22"
close-reason: "Obsolete: Zig rewrite"
---

After compilation completes, verify all function references in the generated code resolve to defined functions. This catches cases where compile-time checking might miss dynamic references. Emit clear error messages listing all undefined functions.
