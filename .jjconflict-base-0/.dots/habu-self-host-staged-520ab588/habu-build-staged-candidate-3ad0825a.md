---
title: Build staged candidate engine
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T23:00:00.515382+02:00"
blocks:
  - habu-self-compile-compiler-b94c5317
---

Full context: build an isolated bin/hb candidate whose compiler modules and application definitions use the staged path while stable bootstrap/recovery boundaries remain explicit. Acceptance: full candidate, AOT, REPL, image, debugger, profiler, snapshot, and cross-target suites pass; candidate never silently invokes the old compiler.
