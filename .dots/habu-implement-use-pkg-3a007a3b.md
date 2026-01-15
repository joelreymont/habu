---
title: Implement use-package primitive
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:41:50.256143+02:00"
---

src/runtime/primitives/package.zig: Add use_package(packages_to_use, package). Add to use-list, inherit external symbols. Dependencies: habu-implement-shadowing-import-96fa0d41. Verify: (use-package 'cl).
