---
title: "Zig infra: Extend GC to scan all object types. Currently gc.zig:129 only scans cons cells. Add scanning for: vectors (scan elements), closures (scan captured vars), symbols (scan plist). Update traceRoots and copy functions."
status: closed
priority: 2
issue-type: task
created-at: "2025-12-25T08:10:49.210792+02:00"
closed-at: "2025-12-25T11:55:50.083027+02:00"
close-reason: GC now scans all object types via work-list
---
