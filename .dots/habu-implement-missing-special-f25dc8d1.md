---
title: Implement missing special operators
status: open
priority: 2
issue-type: task
created-at: "2026-01-17T21:36:00.984642+02:00"
---

compile.zig: Add 3 missing special operators (load-time-value, locally, macrolet, symbol-macrolet). locally is in stdlib but should be special form. macrolet and symbol-macrolet need compiler support for lexical macro env. load-time-value needs eval-when style handling.
