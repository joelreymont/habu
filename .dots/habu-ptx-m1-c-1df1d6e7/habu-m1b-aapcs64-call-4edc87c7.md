---
title: "M1b: AAPCS64 call trampoline"
status: open
priority: 1
issue-type: task
created-at: "2026-06-25T13:48:30.809269+02:00"
blocks:
  - habu-m1a-dynamic-linux-1ff8d288
---

Runtime call helper emitted via src/arch/arm64/asm.f: given (fnaddr, args), set up int args x0-x7, FP args v0-v7, stack spill for args>=9 (16-byte SP), callee-saved x19-x28, BLR, return in x0/x1 or d0/v0. Subset the Driver API needs (int/ptr/float + out-pointers; no HFA/HVA/variadics). Test on zed against a known libc fn (e.g. a math fn for FP, a libc call for int/ptr).
