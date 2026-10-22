---
title: Marshal FFI calls for SysV AMD64
status: closed
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.774308+03:00"
closed-at: "2026-10-22T12:36:34.261853+03:00"
close-reason: "Triage 2026-10-22, DONE: d3fa08e7 \"Marshal SysV FFI calls\", e224283a, 6c3aa999. ffi-abi.f:1-6 covers SysV 6+8 registers. The never-run float32 stubs (HANDOFF gap 6) are covered by pass-the-full."
blocks:
  - habu-add-sysv-abi-75f86980
  - habu-run-bin-hb-6378f297
---

Problem: `src/lib/ffi-abi.f:3-5,563-566` are AAPCS64 (8 register slots each kind).
Acceptance: on `HB-TARGET-LINUX-X86-64?` the planner uses 6 integer and 8 float register slots and stack placement for the rest; `VARIADIC` keeps only its stack-placement meaning because K11a/K11b set `al` on every call, so the `src/lib/aio.f:281,285` `syscall` rows (no `VARIADIC`; `aio.f:275-278` names the seam) need no re-declaration and the seam opens (the `src/lib/aio.f` comment and `docs/aio.md` say so); cases: more than 6 integer arguments, a dirty `al` before a variadic call (the case habu-port-the-ffi-676f745d names), VM registers after a call.
Files: `src/lib/ffi-abi.f`, `src/lib/ffi-test.f`, `src/lib/aio.f` (comment), `docs/aio.md`.
Verify: ThinkPad `hb-x64 --load src/lib/ffi-test.f`; spark `bin/hb --load src/lib/ffi-test.f` unchanged.
Depends: habu-add-sysv-abi-75f86980 (K11b), habu-run-bin-hb-6378f297 (X6), habu-sign-extend-c-c7f55f0e (R1).
Route: Alder (shared: src/lib/ffi-abi.f, src/lib/ffi-test.f, src/lib/aio.f, docs/aio.md).
Ownership: krait (Intel lane).
Claim: unassigned.
