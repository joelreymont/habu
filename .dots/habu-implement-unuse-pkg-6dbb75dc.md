---
title: Implement unuse-package primitive
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:41:55.356555+02:00"
---

src/runtime/primitives/package.zig: Add unuse_package(packages_to_unuse, package). Remove from use-list. Dependencies: habu-implement-use-pkg-3a007a3b. Verify: (unuse-package 'cl).
