---
title: Implement list-all-packages primitive
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:41:10.211044+02:00"
---

src/runtime/primitives/package.zig: Add list_all_packages(). Return list of all registered packages. Dependencies: habu-implement-pkg-use-817f4073. Verify: (list-all-packages) shows CL, CL-USER, etc.
