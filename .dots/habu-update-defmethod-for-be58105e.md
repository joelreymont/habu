---
title: Update defmethod for method qualifiers
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:48.302279+02:00"
---

src/compiler/compile.zig: Parse method qualifiers
- Depends on: dot (method qualifier storage)
- Parse optional qualifier (:before/:after/:around) after method name
- Store qualifier in method metadata
- Register with appropriate qualifier list
- Add tests for qualified method definition
- Est: 20 min
