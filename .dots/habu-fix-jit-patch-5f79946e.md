---
title: Fix JIT patch unpatched holes
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T12:07:54.640509+02:00"
---

src/jit/patch.zig:83 - patchStencil leaves holes unpatched when values short, generates invalid code. Return error on insufficient patch values. Medium severity.
