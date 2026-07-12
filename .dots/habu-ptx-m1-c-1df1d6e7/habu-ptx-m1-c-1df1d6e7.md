---
title: "PTX M1: C-ABI FFI + CUDA Driver harness"
status: open
priority: 1
issue-type: task
created-at: "\"2026-06-25T13:43:16.897180+02:00\""
---

docs/ptx-sketch.md milestone 1 (prerequisite, large). AAPCS64 call trampoline (int x0-x7, FP v0-v7, x8 indirect-result, stack spill, callee-saved x19-x28), out-param/void** kernelParams marshalling + readback, first-symbol bootstrap (resolve dlopen/dlsym before FFI exists), Tegra-path libcuda, Driver harness (cuInit/cuDevicePrimaryCtxRetain/cuMemAlloc/cuMemcpy/cuModuleLoad/cuModuleGetFunction/cuLaunchKernel) + launch-ABI check. Resolve the M1 Open Design Questions (kernel ABI, first-symbol mechanism, context interop) FIRST, then decompose into sub-dots. Built/run on the Orin (zed).
