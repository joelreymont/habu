---
title: Canonicalize package state
status: closed
priority: 1
issue-type: task
created-at: "\"2026-04-01T22:06:02.103695+02:00\""
closed-at: "2026-04-03T09:07:08.168997+02:00"
close-reason: done (zig build; zig build test back to known 5-error baseline)
blocks:
  - habu-fix-pkg-bootstrap-61bed166
---

Problem: package export/import/current-package state has multiple mutable sources of truth. Acceptance: one canonical package state drives lookup and package context never silently resets. Files: src/runtime/primitives/package.zig:316-330,642-656,1711-1716; src/runtime/heap.zig:3326-3339. Verify: focused package state regressions and GC-preserved current-package tests. Blockers: habu-fix-pkg-bootstrap-61bed166; also depends on habu-remove-legacy-lookup-e81bb093 and habu-make-lookup-apis-32266bf5.
