---
title: Add streamp primitive
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:38:57.653509+02:00"
---

src/runtime/primitives/io.zig: Add streamp() predicate. Return t if value is stream. Dependencies: habu-add-stream-type-0f7c9ae0. Verify: (streamp (make-string-input-stream "hi")) => t
