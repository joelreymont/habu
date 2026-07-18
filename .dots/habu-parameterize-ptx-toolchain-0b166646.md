---
title: Parameterize PTX toolchain target arch (sm_87 hardcoded)
status: open
priority: 1
issue-type: task
created-at: "2026-07-18T16:35:29.646919+02:00"
---

Codegen-verdict finding: lib/ptx/toolchain.f:65 hardcodes -arch=sm_87, so cuModuleLoad fails rc 209 NO_BINARY_FOR_GPU on GB10; proven one-token fix -arch=sm_121a (+ PTXAS=/usr/local/cuda/bin/ptxas; default cuda-12.6 path dead) makes the identical PTX/driver path compute correctly. Proper fix: target arch from the process row (docs/tma-gather.md piece 4), not a constant — sm_87 on zed, sm_121a on spark; ptxas path per host. UNBLOCKS all E1 device-gated dots on spark. Also follow-up from verdict: bandwidth-lib.f N=2^20 is L2-resident on GB10 (measures 1516 GB/s) — true DRAM roof needs N>=64M.
