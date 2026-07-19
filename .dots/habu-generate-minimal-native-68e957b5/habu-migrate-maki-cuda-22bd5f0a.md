---
title: Migrate Maki CUDA lifecycles
status: open
priority: 1
issue-type: task
created-at: "2026-07-19T20:52:37.079374+02:00"
---

maki/gpu.f SETUP/LAUNCH/RELEASE, maki/lower/launch.f LLA-CTX-OPEN/LLA-MOD-OPEN/LLA-ALLOC-UPLOAD/LLA-RELEASE, and maki/eval/emit-device.f CU-OPEN plus its softmax/GEMM/attention allocators all acquire CUDA resources into globals and invoke release only after the full happy path. Concrete failures: a second CUMEMALLOC failure leaks the first allocation; copy/bind/launch/sync/readback throws leak every allocation, module, and retained primary context; module-get-function failure leaks the loaded module/context; and the first cleanup RC0 throw prevents later buffers/module/context from being released. Several handle cells are not cleared, so reused runner state can double-release stale values. Migrate these Maki paths to habu-add-exc-safe-74d2f76e scopes. Stage all host-only validation before acquisition, own the PTXTC temporary root in the same unwind boundary where applicable, transfer only the outputs that legitimately outlive a call, and make repeated runs after injected failure clean. Add injected failure matrices at every driver call plus real off-device lifecycle tests; prove outstanding-resource count returns to zero, primary error identity is preserved, cleanup-only errors propagate, cleanup order is reverse acquisition, and whole-model multi-region execution keeps only the intended context/buffers alive between regions. Files: maki/gpu.f/tests, maki/lower/launch.f/tests, maki/eval/emit-device.f/tests. Depends: habu-add-exc-safe-74d2f76e. Ownership: Maki CUDA caller migration only; no emitter, ABI, or numerical changes.
