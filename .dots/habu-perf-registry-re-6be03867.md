---
title: "Perf registry: re-measure seeded rows on-device, retire ATTENTION waiver"
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-14T15:16:58.889338+02:00\""
---

Problem: tools/ptx/perf-rows.tsv carries orin-nx-15w rows measured in early July plus an ATTENTION WAIVER row (value 0) that owes a real measured profile row - the kernel-perf workflow's own rule is that waivers are visible debt. With zed available and the fail-closed device-proof work (habu-make-ptx-device-c0eb12a3) landed, run the benchmark/profile harness on-device on current master: re-measure the seeded SAXPY/RELU/GEMM-ladder rows, add the real ATTENTION profile row, and let PERF:SCAN judge regressions against the old rows (a real regression = investigate, not overwrite). Acceptance: ATTENTION WAIVER row replaced by a measured row; every re-measured row appended with today's date; perf-regress green or regressions dotted; registry validated by perf-registry-test. Files: tools/ptx/perf-rows.tsv (+ any harness fixes discovered). Verify: on-device tools/ptx/perf-regress.f + gemm-bench/fusion-compare runs; host-side perf-registry-test.f. Depends: habu-make-ptx-device-c0eb12a3 (trustworthy device rc handling), zed provisioning. Ownership: perf registry rows. Claim: unassigned.

Claim: agent=perfzed workspace=.jj-ws/fable-perfzed

RESCOPE 2026-07-14 (user decision): 25W is the CANONICAL measurement
environment - do NOT reboot to 15W. First on-device pass (25W, GPU pinned
918 MHz) proved the harness end-to-end: fusion-compare self-emit exit 0
(SAXPY-V4 93458, RELU-V4 91811, UNFUSED 92792, FUSED 93341 GB/s_x1000,
ratio 1676), gemm-bench exit 0 (MM up to 981286, MMM 885873, MMN 107886
GFLOP/s_x1000 at 2048^3) - c0eb12a3's on-device fusion evidence discharged.
The box is reboot-locked in nvpmodel mode 3 (25W, 4 TPCs/8 SMs,
TPC_PG_MASK=240); it was found with GPU DVFS pinned at 408 MHz (restore
after measuring). New scope: (1) re-measure the seeded kernels at 25W and
APPEND rows tagged orin-nx-25w dated with the measurement day (15W rows stay
as history; a new device tag is a fresh baseline, so PERF:SCAN/perf-regress
must not fabricate cross-tag regressions - verify, fix if needed); (2) pin
and DOCUMENT the canonical 25W measurement clock (determine mode-3 max from
the box; first pass used 918 MHz); (3) update docs/kernel-principles.md
"THIS device" measured-roof section for 25W/8-SM (re-measure memory B and
FP32 roof); (4) ATTENTION waiver retirement stays pending on the
attention-bench producer dot (no benchmark tool exists - only the emitter)
plus perf-registry-test.f PRT-COMMITTED-TESTS flip (asserts the WAIVER row
is present; must assert the measured row instead when it retires).
