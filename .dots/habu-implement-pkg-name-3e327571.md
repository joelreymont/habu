---
title: Implement package-name primitive
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:40:54.713905+02:00"
---

src/runtime/primitives/package.zig: Add package_name(pkg). Return package name string. Dependencies: habu-implement-find-pkg-cee7a2f6. Verify: (package-name (find-package "CL")).
