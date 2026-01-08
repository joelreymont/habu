---
title: Implement do and do* macros
status: open
priority: 2
issue-type: task
created-at: "2026-01-08T06:25:05.625578+02:00"
---

File: lib/stdlib.habu - Standard CL iteration macros missing. (do ((var init step)*) (end-test result-form*) declaration* body*) and do* (sequential binding). These are fundamental iteration constructs in CL, more flexible than loop. Need: variable binding/stepping, end test, implicit tagbody in body, proper scope handling.
