---
title: "M1d: CUDA Driver harness + launch-ABI"
status: open
priority: 2
issue-type: task
created-at: "2026-06-25T13:48:30.821016+02:00"
blocks:
  - habu-m1c-marshalling-out-99ccd38c
---

dlopen the Tegra libcuda by absolute path; dlsym + wrap as checked Forth words: cuInit, cuDevicePrimaryCtxRetain (NOT cuCtxCreate - the camera pipeline owns a context), cuMemAlloc/Free, cuMemcpyHtoD/DtoH, cuModuleLoadDataEx, cuModuleGetFunction, cuLaunchKernel; the launch-ABI check (blockDim==B, gridDim.x==R, grid*block<=2^32-1, flat coverage). Validate on zed: cuInit + alloc/copy/free round-trip.
