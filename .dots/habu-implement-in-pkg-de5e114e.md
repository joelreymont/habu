---
title: Implement in-package special form
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:42:28.259827+02:00"
---

src/compiler/compile.zig: Add compileInPackage around line ~3800. Parse (in-package name). Compile to set *package* at compile/load time. Dependencies: habu-add-pkg-special-cca025fd. Verify: (in-package :foo) switches packages.
