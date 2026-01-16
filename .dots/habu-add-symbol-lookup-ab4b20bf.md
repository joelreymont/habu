---
title: Add symbol lookup primitives
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:31.297870+02:00"
---

src/runtime/primitives/package.zig: Implement symbol lookup
- intern: find or create symbol in package
- unintern: remove symbol from package
- find-symbol: lookup symbol, return (values symbol status)
- find-all-symbols: find symbol in all packages
- Status: :internal/:external/:inherited/nil
- Add tests for lookup results
- Est: 25 min
