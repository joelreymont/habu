---
title: Close CL semantic gaps
status: closed
priority: 1
issue-type: task
created-at: "\"2026-02-17T22:23:04.163786+01:00\""
closed-at: "2026-02-20T00:08:20.691004+01:00"
close-reason: all child semantic gaps closed (function designators, package semantics, macro expansion)
blocks:
  - habu-audit-loader-failures-fda25dca
---

lib/stdlib.habu and src/compiler/compile.zig. Cause: missing generic CL semantics blocks Maxima macros. Fix: implement spec-correct function designators, setf places, macroexpansion, package semantics.
