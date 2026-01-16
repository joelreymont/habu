---
title: Add package modification primitives
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:30.688547+02:00"
---

src/runtime/primitives/package.zig: Implement package modification
- delete-package: remove package from system
- rename-package: change package name/nicknames
- unexport: remove symbols from export list
- shadow: create shadowing symbol
- shadowing-import: import and shadow symbol
- Add tests for package state changes
- Est: 25 min
