---
title: "M1c: marshalling + out-params + kernelParams"
status: closed
priority: 2
issue-type: task
created-at: "\"2026-06-25T13:48:30.815537+02:00\""
closed-at: "2026-06-29T10:00:42.357374+02:00"
close-reason: "completed: split target-independent FFI ABI into lib/ffi-abi.f; added checked out-param and kernelParams helpers with focused tests; documented Linux-only DLOPEN slots versus portable macOS ABI proof; full native gate passed 58732ms <= 70000ms before tracker close"
blocks:
  - habu-m1b-aapcs64-call-4edc87c7
---

Forth cell <-> C int/ptr/float; caller-allocated out-pointer scratch + readback (cuMemAlloc CUdeviceptr*, cuModuleGetFunction CUfunction*); void** kernelParams array packing matching a kernel's .param block + lifetimes.
