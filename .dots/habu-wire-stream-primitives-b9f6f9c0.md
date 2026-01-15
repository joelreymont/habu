---
title: Wire stream primitives to compiler
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:40:21.401553+02:00"
---

src/compiler/compile.zig: Add builtin symbols for all 17 stream functions around line ~300-320. Add dispatch cases. Map to primitive calls. Dependencies: habu-implement-with-output-3cfb34ea. Verify: all stream ops compile.
