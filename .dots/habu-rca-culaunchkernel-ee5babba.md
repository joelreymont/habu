---
title: RCA cuLaunchKernel kernelParams fault via FFI-CALLN
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T10:17:19.099423+02:00"
---

The new lib/ffi.f FFI-CALLN handles >8 args (proven: 10-arg FFI-T-SUM10=55, and cuLaunchKernel's 11-arg call returns rc=0). But launching SAXPY via cuLaunchKernel(f,1,1,1,256,1,1,0,0,kernelParams,0) makes cuCtxSynchronize HANG, while the deprecated cuParamSetv/cuFuncSetBlockShape/cuLaunchGrid path runs correct-vs-golden every time. kernelParams = [&PK-DX,&PK-DY,&PK-ABITS,&PK-NV] (addresses of the device-ptr/a-bits/n cells); param order matches the cubin (p_x .u64, p_y .u64, p_a .f32, p_n .u32, 24B packed). Launch rc=0 means the driver accepted/read the params, so the kernel itself faults/spins. ptrace_scope blocks gdb attach; use cuda-gdb or the baked Forth stepper (docs/debugging.md). Files: probe /tmp/clk-probe.f; target tools/ptx/cuda-launch.f + maki/gpu.f. Goal: replace the deprecated <=8-arg launch API with cuLaunchKernel once the kernelParams fault is root-caused. Until then the deprecated API stays (proven, golden PASS). FFI-DLBUF footgun fix is already in use automatically.
