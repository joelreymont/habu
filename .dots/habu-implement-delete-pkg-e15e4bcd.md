---
title: Implement delete-package primitive
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:42:11.343520+02:00"
---

src/runtime/primitives/package.zig: Add delete_package(package). Remove from global registry, unuse from all packages. Dependencies: habu-implement-unintern-primitive-b5a56dd2. Verify: (delete-package pkg).
