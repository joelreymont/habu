---
title: "CAD: gate-licensed precision policy (TF32/FP16)"
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-04T19:28:43.768533+02:00\""
---

CAD-PLAN 8.1 lever 5. Precision demotion (f32 -> TF32 accumulate / fp16 storage) applied per-region only where the gates PROVE it: GOLDEN passes under the demoted precision tolerance AND gradcheck stays within its class tolerance; the evidence row records the licensed precision. Never a global flag; a per-region, per-artifact licensed fact in the store. Needs: dtype variants in the schedule families (gemm-tf32 exists as the first), tolerance policy rows per precision (extend section 11 defaults), and PROMOTE recording the license. Depends: tensor-core MMA emitters + cad-6-tune. Blocks: compute-roof parity with honest numerics.

LANDED 2026-07-05 (step 3a, the MMA prerequisite): maki/precision.f registry (PREC-F32/PREC-TF32, per-class request PREC! ( prec class -- ), PREC-RESET, fail-closed unlicensed pairs E-PREC-ROW); tf32 matmul row atol 1e-6 / rtol 2e-3 (measured ~7.9e-4 TF32 GEMM error, ~2.5x headroom, docs/eval-triton.md); lower-golden judges under the ACTIVE row and names it in the reason + LG-PREC-USED@; evidence rows extend to golden=device-pass:<prec> (store.f EVID-PUT-G + STORE-P$, host legs unchanged); device proof maki/precision-device-test.f (tf32 pass + evidence + 0.5% inverse guard + reset). REMAINING for this dot: fp16/bf16 tolerance rows with a measured derivation, gradcheck judged under the demoted precision, schedule-family dtype variants, and the MMA emitters actually consuming the license (habu-tensor-core-mma).
