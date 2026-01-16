---
title: Add package query primitives
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:30.385852+02:00"
---

src/runtime/primitives/package.zig: Implement package queries
- list-all-packages: return list of all packages
- package-name: get package name string
- package-nicknames: get list of nickname strings
- package-use-list: packages this package uses
- package-used-by-list: packages that use this package
- package-shadowing-symbols: list of shadowing symbols
- Add tests for package relationships
- Est: 20 min
