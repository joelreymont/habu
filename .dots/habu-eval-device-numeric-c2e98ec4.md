---
title: Eval device numeric goldens for sumnorm/gemm/attention (closes wrong-but-green class)
status: open
priority: 2
issue-type: task
created-at: "2026-07-13T18:37:35.702154+02:00"
---

Problem: the off-device authoring autograder (maki/eval-emit.f) grades same-type semantic bugs GREEN(2) — sumnorm in/out swap, div-by-sum-squared, gemm double-accumulate (2*A*B), attention Q/K swap and output-into-V all certify AND emit the required PTX instructions, so the STRUCTURAL required/forbidden gates cannot distinguish them from a correct kernel (pinned as acknowledged wrong-but-green regressions in maki/eval-emit-test.f). Only a NUMERIC device golden (run the emitted PTX on the Orin, compare outputs to a CPU reference) closes this class. Fix: add device numeric goldens for sumnorm, gemm, attention that FAIL each pinned wrong-but-green shape and pass the correct kernel; wire behind the device-FFI SKIP so host gates skip. E1 device-gated (needs Orin). Acceptance: on-device, each wrong-but-green shape's output diverges from the CPU reference beyond tolerance; the correct kernel matches. Files: maki/eval-device.f + a new eval-emit-device golden, maki/eval-emit-test.f (flip the pins to device-caught when it lands). Verify: on-device golden run. Depends: none (device leg). Ownership: eval device goldens. Claim: unassigned (E1 device-gated).

ON-DEVICE FINDING 2026-07-14 (zed, first real device gate run): even ON the
Orin with libcuda present, the eval-matrix legs still self-skip - eval-emit/
eval-live-author print "device leg SKIPPED (Orin-gated)" and matrix rows show
device=not-run, GB/s-x10=not-run. The SKIP is not keyed on device presence, so
this dot also needs the opt-in mechanism (key the leg on the device-FFI probe
that maki/device-smoke.f already proves, not on an ambient always-off gate).
