---
title: Implement shadowing-import primitive
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:41:45.239265+02:00"
---

src/runtime/primitives/package.zig: Add shadowing_import(symbols, package). Import + shadow combo. Dependencies: habu-implement-shadow-primitive-915ee661. Verify: (shadowing-import 'foo).
