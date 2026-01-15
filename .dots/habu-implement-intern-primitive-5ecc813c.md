---
title: Implement intern primitive
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:41:21.416141+02:00"
---

src/runtime/primitives/package.zig: Add intern_symbol(name, package). Intern symbol in package, return symbol + status (:internal/:external/:inherited/nil). Dependencies: habu-update-symbol-interning-1b96c4b5. Verify: (intern "FOO" *package*).
