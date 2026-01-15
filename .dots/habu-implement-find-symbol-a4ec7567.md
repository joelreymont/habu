---
title: Implement find-symbol primitive
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:41:26.073070+02:00"
---

src/runtime/primitives/package.zig: Add find_symbol(name, package). Search package + use-list. Return symbol + status or nil. Dependencies: habu-implement-intern-primitive-5ecc813c. Verify: (find-symbol "CAR").
