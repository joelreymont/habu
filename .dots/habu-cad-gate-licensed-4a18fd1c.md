---
title: "CAD: gate-licensed precision policy (TF32/FP16)"
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-04T19:28:43.768533+02:00\""
---

CAD-PLAN 8.1 lever 5. Precision demotion (f32 -> TF32 accumulate / fp16 storage) applied per-region only where the gates PROVE it: GOLDEN passes under the demoted precision tolerance AND gradcheck stays within its class tolerance; the evidence row records the licensed precision. Never a global flag; a per-region, per-artifact licensed fact in the store. Needs: dtype variants in the schedule families (gemm-tf32 exists as the first), tolerance policy rows per precision (extend section 11 defaults), and PROMOTE recording the license. Depends: tensor-core MMA emitters + cad-6-tune. Blocks: compute-roof parity with honest numerics.
