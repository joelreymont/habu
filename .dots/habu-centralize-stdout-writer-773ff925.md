---
title: Centralize stdout writer setup
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T12:08:03.155458+02:00"
---

src/runtime/primitives/io.zig:40 - Repeated stdout writer setup. Centralize writer helper. Low severity.
