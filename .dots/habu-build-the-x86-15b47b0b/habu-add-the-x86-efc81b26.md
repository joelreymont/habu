---
title: Add the x86-64 task entry
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T13:12:28.897036+03:00"
---

Problem: `BTASK-ENTRY` (`src/habu/habu1.f:2099`) is ARM64 code. Split from habu-port-the-ffi-676f745d (task entry).
Acceptance: `task-entry` synthesises the pthread entry stack and VM registers; the return path and state restoration follow `habu1.f:2099` semantics; the rows join the `docs/x86-64.md` kernel inventory.
Files: `src/habu/boot-x64.f`, `src/habu/kernel-x64.f`, `test/x86-64-peer-routines.f`, `docs/x86-64.md` (kernel inventory).
Verify: ThinkPad: a routine image that starts a pthread through `task-entry` and returns.
Depends: habu-add-sysv-ffi-17a130a1 (K11a).
Route: direct.
Ownership: krait (Intel lane).
Claim: unassigned.
