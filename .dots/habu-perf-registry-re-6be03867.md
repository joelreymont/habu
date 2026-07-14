---
title: "Perf registry: re-measure seeded rows on-device, retire ATTENTION waiver"
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-14T15:16:58.889338+02:00\""
---

Problem: tools/ptx/perf-rows.tsv carries orin-nx-15w rows measured in early July plus an ATTENTION WAIVER row (value 0) that owes a real measured profile row - the kernel-perf workflow's own rule is that waivers are visible debt. With zed available and the fail-closed device-proof work (habu-make-ptx-device-c0eb12a3) landed, run the benchmark/profile harness on-device on current master: re-measure the seeded SAXPY/RELU/GEMM-ladder rows, add the real ATTENTION profile row, and let PERF:SCAN judge regressions against the old rows (a real regression = investigate, not overwrite). Acceptance: ATTENTION WAIVER row replaced by a measured row; every re-measured row appended with today's date; perf-regress green or regressions dotted; registry validated by perf-registry-test. Files: tools/ptx/perf-rows.tsv (+ any harness fixes discovered). Verify: on-device tools/ptx/perf-regress.f + gemm-bench/fusion-compare runs; host-side perf-registry-test.f. Depends: habu-make-ptx-device-c0eb12a3 (trustworthy device rc handling), zed provisioning. Ownership: perf registry rows. Claim: unassigned.

Claim: agent=perfzed workspace=.jj-ws/fable-perfzed
