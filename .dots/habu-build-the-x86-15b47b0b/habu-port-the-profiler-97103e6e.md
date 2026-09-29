---
title: Port the profiler tick and alternate stack
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T13:12:28.880210+03:00"
blocks:
  - habu-port-signals-crash-2c7768ca
---

Problem: the profiler's sampling handler models aarch64 frames only (`src/habu/prof.f:47-50` refuses other targets; `prof.f:867-918` is the handler and alternate-stack contract).
Acceptance: an x86 sampling handler reading `RIP`, `sigaltstack`, a refused mapping named and fatal (`prof.f:867-918` semantics); the rows join the `docs/x86-64.md` kernel inventory.
Files: `src/habu/boot-x64.f`, `test/x86-64-peer-routines.f`, `docs/x86-64.md` (kernel inventory).
Verify: ThinkPad: a routine image taking profiler ticks on the alternate stack.
Depends: habu-port-signals-crash-2c7768ca (K10a).
Route: direct.
Ownership: krait (Intel lane).
Claim: unassigned.
