---
title: Implement find-package primitive
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:40:50.566081+02:00"
---

src/runtime/primitives/package.zig: Add find_package(name). Lookup in global registry by name or nickname. Dependencies: habu-implement-make-pkg-0cff90fe. Verify: (find-package "COMMON-LISP").
