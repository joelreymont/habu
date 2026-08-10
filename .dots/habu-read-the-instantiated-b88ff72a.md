---
title: Read the instantiated width of a parametric family
status: open
priority: 2
issue-type: task
created-at: "2026-08-10T06:55:29.411661+02:00"
---

A parametric family instantiated with a multi-cell argument (option<pt>) occupies more cells than its declaration reserves; the checker records the difference as WF-XPAD and the engine's emitter reads it at pass 2; the chain cannot ask, so MATCH over such a value refuses E-NELAB-MATCH by name (fail-closed, real fixture in native-match.f, match lane 2026-08-10). Export the fact the way EN.E travelled (a narrow checker export reading a stored fact), thread it through NFAM, and lift the refusal with an executing fixture over an instantiated family. Files: src/core/checker.f (export), src/compiler/native/{family,elaborate}.f. Depends: none.
