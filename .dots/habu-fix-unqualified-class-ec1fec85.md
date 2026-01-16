---
title: Fix unqualified class metadata keys
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T12:07:35.173530+02:00"
---

src/compiler/compile.zig:5606 - Heap class metadata keyed by unqualified name causes package collisions. Store qualified name. Medium severity.
