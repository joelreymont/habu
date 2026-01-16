---
title: Add nsublis primitive
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:24.031312+02:00"
---

src/runtime/primitives/list.zig: Implement nsublis function
- nsublis: destructive version of sublis
- Modify tree structure in place
- Support :test/:test-not/:key parameters
- Reuse cons cells where possible
- Add tests verifying mutation
- Est: 20 min
