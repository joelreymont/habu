---
title: Give x86-64 and Wasm a tier 0
status: open
priority: 3
issue-type: task
created-at: "2026-10-26T11:28:42.504868+02:00"
blocks:
  - habu-keep-the-innermost-636aeac0
---

Retirement prerequisite (Joel 2026-10-26): do only if the experiment ruling retires tier 1. x86-64 set-tier refuses anything but 1 (src/habu/kernel-x64.f:3983-3995); Wasm links tier 1 output.
