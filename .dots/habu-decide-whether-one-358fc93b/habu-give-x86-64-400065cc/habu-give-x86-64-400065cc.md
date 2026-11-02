---
title: Give x86-64 and Wasm a tier 0
status: closed
priority: 3
issue-type: task
created-at: "2026-10-26T11:28:42.504868+02:00"
closed-at: "2026-11-02T11:34:09.255762+02:00"
close-reason: "x86-64 landed: 27e3cf87 'Fill T0's x86 emitter, add fixed bodies', 120d2f547a 'Make T0 the x86-64 runtime compiler', 47a9d0d5 'Run tier-0 test cases on x86'. Wasm's tier 0 continues as habu-emit-wasm-from-0d5dab47."
blocks:
  - habu-decide-whether-one-358fc93b
---

Retirement prerequisite (Joel 2026-10-26): do only if the experiment ruling retires tier 1. x86-64 set-tier refuses anything but 1 (src/habu/kernel-x64.f:3983-3995); Wasm links tier 1 output.

Claim: jerry, 2026-10-27; design in jerry-aot/.out/notes, lanes in jerry-x64-* workspaces.
