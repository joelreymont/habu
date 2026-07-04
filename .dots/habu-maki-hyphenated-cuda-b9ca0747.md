---
title: "Maki: hyphenated CUDA FFI binding names"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T11:22:53.320232+02:00"
---

User FIXME (maki/gpu.f:40): CUDA driver bindings are named CUDEVICEGET / CUDEVICEPRIMARYCTXRETAIN etc. (maki/cuda-driver.f, e.g. line 45: FFI: CUDEVICEGET ( ptr a idx -- rc ) SYMBOL cuDeviceGet FFI;). The Forth word name is independent of the C symbol - the SYMBOL clause carries the exact C name - so nothing blocks repo-standard hyphenation: rename to CU-DEVICE-GET, CU-INIT, CU-MEM-ALLOC, CU-MODULE-LOAD, CU-DEVICE-PRIMARY-CTX-RETAIN... with SYMBOL strings unchanged. Mechanical: rename every FFI: binding in maki/cuda-driver.f + all call sites (maki/gpu.f, eval-device.f, eval-device-sm.f, device-smoke.f); keep the CUDA: package prefix; gate = maki suite + device-smoke on zed. Pure readability/style per docs/forth.md hyphen rule; no behavior change.
