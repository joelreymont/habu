---
title: Implement package-use-list primitive
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:41:03.764810+02:00"
---

src/runtime/primitives/package.zig: Add package_use_list(pkg). Return list of packages this package uses. Dependencies: habu-implement-pkg-nicknames-00950b89. Verify: (package-use-list *package*).
