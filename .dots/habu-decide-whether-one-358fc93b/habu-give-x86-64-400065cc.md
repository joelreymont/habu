---
title: Give x86-64 and Wasm a tier 0
status: active
priority: 3
issue-type: task
created-at: "2026-10-26T11:28:42.504868+02:00"
blocks:
  - habu-decide-whether-one-358fc93b
---

Retirement prerequisite (Joel 2026-10-26): do only if the experiment ruling retires tier 1. x86-64 set-tier refuses anything but 1 (src/habu/kernel-x64.f:3983-3995); Wasm links tier 1 output.

Claim: jerry, 2026-10-27; design in jerry-aot/.out/notes, lanes in jerry-x64-* workspaces.
