---
title: Implement rename-package primitive
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:42:16.085046+02:00"
---

src/runtime/primitives/package.zig: Add rename_package(package, new_name, new_nicknames). Update registry keys. Dependencies: habu-implement-delete-pkg-e15e4bcd. Verify: (rename-package pkg "NEW").
