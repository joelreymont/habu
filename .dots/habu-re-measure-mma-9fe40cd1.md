---
title: Re-measure MMA rows at pinned 918 MHz clocks
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-15T22:11:50.760907+02:00\""
---

Residual from habu-mma-larger-bk-1ae1c6b2 (2026-07-15): all new MMA sweep rows were measured at the UNBOOSTED 408 MHz GPU clock (min=max=cur as-found) because the lane's harness refused a clock pin as a device change; the repo's shipped competitive numbers (HABU-MMM-TF32 884.9 GFLOP/s, fusion-compare family) were measured at 918 MHz per STATUS.md/registry history, so the absolute numbers are not comparable (relative +54.8% swizzle win is clock-independent and stands). Fix (device, zed): sanctioned clock-pin procedure - record as-found clock state (min/max/cur), pin to the historical 918 MHz measurement clock via jetson_clocks or the sysfs devfreq knob WITHOUT touching nvpmodel/25W mode or rebooting, re-run gemm-bench for the baseline + BK=32 pad=8 + BK=64 pad=8 dyn configs, record honest orin-nx-25w-918mhz-tagged rows, then RESTORE the as-found clock state and verify identical. Then refresh the shipped competitive comparison (eval-triton HABU-MMM-TF32 row) if the swizzled config at 918 MHz materially moves it - via the typed BENCH import path, policy-comparable rows only. Files: tools/ptx/perf-rows.tsv, possibly tools/eval-triton.f fixture + docs/eval-triton.md. Verify: mma-gemm-check element-exact at the pinned clock, rows parse, kernel-perf-lint. Ownership: ptx perf evidence. NOTE: if the user prefers to approve clock pinning explicitly, hold this dot for their go-ahead.

Claim: agent=clockbench workspace=.jj-ws/fable-clockbench
