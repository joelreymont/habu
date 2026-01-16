---
title: Implement qualified method dispatch
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:47.694196+02:00"
---

src/interp/vm.zig: Update method invocation for qualifiers
- Depends on: dot (method qualifier storage)
- Invoke around methods first (most specific to least)
- Invoke before methods (most to least specific)
- Invoke most specific primary method
- Invoke after methods (least to most specific)
- Add tests for invocation order
- Est: 30 min
