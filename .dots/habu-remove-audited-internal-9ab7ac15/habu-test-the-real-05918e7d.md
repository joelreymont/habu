---
title: Test the real bootstrap lookup
status: closed
priority: 2
issue-type: task
created-at: "2026-10-16T11:17:41.853490+03:00"
closed-at: "2026-10-22T12:36:35.367135+03:00"
close-reason: "Triage 2026-10-22, DONE: tools/bootstrap.sh:155 appends src/habu/hide.f itself, and tools/bootstrap-codegen-test.f:8 includes the same file: no twin."
---

hide.f BFR twin is tested while bootstrap.sh emits separately maintained BOOT implementation. Share actual bootstrap lookup source or exercise generated prefix; preserve first match, case folding, duplicate/missing marker behavior. Remove duplicate inverse implementation. Existing bootstrap-codegen cases and no-binary recovery check.
