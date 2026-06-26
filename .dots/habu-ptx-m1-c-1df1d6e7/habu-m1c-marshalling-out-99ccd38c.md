---
title: "M1c: marshalling + out-params + kernelParams"
status: open
priority: 2
issue-type: task
created-at: "2026-06-25T13:48:30.815537+02:00"
blocks:
  - habu-m1b-aapcs64-call-4edc87c7
---

Forth cell <-> C int/ptr/float; caller-allocated out-pointer scratch + readback (cuMemAlloc CUdeviceptr*, cuModuleGetFunction CUfunction*); void** kernelParams array packing matching a kernel's .param block + lifetimes.
