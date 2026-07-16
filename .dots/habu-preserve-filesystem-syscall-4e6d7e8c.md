---
title: Preserve filesystem syscall failures
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-16T12:13:06.537842+02:00\""
---

Full cold gate reproducibly fails native engine build and nested tail-process loaders with E-FS-OPEN while each standalone phase passes. Current Darwin open/open-rd primitives collapse every syscall failure to -1, so the gate cannot distinguish missing paths from EMFILE/ENFILE or report the failing path; filesystem helpers replace raw cause with E-FS-OPEN. Prove raw syscall result and path on the exact full-gate command, add reusable checked diagnostics and negative tests, fix the root resource/loader invariant, then prove cold full gate green. Depends on active candidate/PTTY integration only for final combined gate.
