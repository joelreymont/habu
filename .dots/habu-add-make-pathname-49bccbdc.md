---
title: Add make-pathname primitive
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:37.067958+02:00"
---

src/runtime/primitives/io.zig: Implement make-pathname
- Depends on: dot (Pathname object type)
- make-pathname: create pathname from keyword args
- Support :host/:device/:directory/:name/:type/:version/:defaults
- Merge with defaults pathname if provided
- Add tests for various pathname forms
- Est: 25 min
