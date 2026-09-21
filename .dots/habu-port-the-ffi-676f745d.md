---
title: Port the FFI, task entry and traps to SysV AMD64
status: open
priority: 2
issue-type: task
created-at: "2026-09-17T17:32:42.546263+03:00"
---

Problem: docs/x86-64.md (campaign habu-campaign-c6-targets-86bb56bb): lib/ffi-abi.f marshals for AAPCS64, BTASK-ENTRY is ARM64 code, and the signal handler decodes traps from PC and SP. Acceptance: SysV AMD64 marshalling in lib/ffi-abi.f selected by target (integer and pointer arguments in rdi rsi rdx rcx r8 r9, the stack for the rest, rax for the return, 16-byte stack alignment at the call); because the callee-saved set (rbx rbp r12-r15) is the VM register set, a foreign call preserves the VM without a save block and a test proves the registers after a call; the task entry and the guard-page model for x86_64; the signal frame decoding reads RIP and RSP; the FFI, task and guard-page suites green on the peer. Files: lib/ffi-abi.f, lib/task.f, src/os/linux-x86-64/sign.f, src/habu/ (task entry), test/. Verify: the FFI, task and guard-page suites on the Intel machine through the peer gate. Depends: the lowering, the cross-build. Ownership: Joel (x86_64 lane). Claim: unassigned. Added at the AIO landing: SysV makes libc `syscall` a true variadic call, and a variadic callee reads `al` as the count of vector registers used, so the x86-64 marshalling zeroes `al` before every call to a variadic symbol (zero vector arguments in every FUNCTION: row today); lib/aio.f's two `syscall` rows (io_uring_setup 425, io_uring_enter 426) are the first consumers and docs/aio.md names the seam; a case calls a variadic libc symbol with `al` left dirty by the preceding code and proves the marshalling cleared it.
