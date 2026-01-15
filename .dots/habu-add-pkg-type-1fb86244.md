---
title: Add package type predicates
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:40:33.935356+02:00"
---

src/runtime/value.zig: Add isPackage(), asPackage() methods. Update typeKind() enum. Dependencies: habu-design-pkg-obj-44df0b34. Verify: value.isPackage() compiles.
