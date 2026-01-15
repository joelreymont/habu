---
title: Add stream type predicates
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:38:52.383089+02:00"
---

src/runtime/value.zig: Add isStream(), asStream() methods. Update typeKind() enum. Dependencies: habu-design-stream-obj-270e828e. Verify: value.isStream() compiles.
