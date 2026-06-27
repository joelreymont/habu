---
title: "M1b: AAPCS64 call trampoline"
status: closed
priority: 1
issue-type: task
created-at: "\"2026-06-25T13:48:30.809269+02:00\""
closed-at: "2026-06-27T14:11:16.201087+02:00"
close-reason: "completed: implemented checked FFI ABI helpers plus native AAPCS64 trampoline for x0-x8, d0-d7, stack spill, x0 int/pointer returns, d0 float returns, and x8 indirect-result pointer; validated with lib/ffi-test.f, native fixpoint, warm tool gate, and full native gate 90.76s. Two-register aggregate returns are outside the CUDA Driver subset and remain outside M1b."
blocks:
  - habu-m1a-dynamic-linux-1ff8d288
---

Runtime call helper emitted via src/arch/arm64/asm.f: given (fnaddr, args), set up int args x0-x7, FP args v0-v7, stack spill for args>=9 (16-byte SP), callee-saved x19-x28, BLR, return in x0/x1 or d0/v0. Subset the Driver API needs (int/ptr/float + out-pointers; no HFA/HVA/variadics). Test on zed against a known libc fn (e.g. a math fn for FP, a libc call for int/ptr).
