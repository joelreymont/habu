---
title: Widen LMM-EPI-OP? to scale/bias matmul epilogues
status: open
priority: 2
issue-type: task
created-at: "2026-07-15T19:36:42.257026+02:00"
---

Found by the gemmdev lane 2026-07-15 (executable fail-closed characterization committed in maki/onnx/deploy-composed-device-test.f): a composed Gemm with alpha<>1 or a separate bias inserts OP-SCALE/OP-BIAS nodes that FP-BUILD fuses into the matmul region as epilogue members, but maki/lower-mm.f LMM-EPI-OP? accepts only relu/gelu/silu, so those regions reject E-LMM-OP (-5194) - alpha<>1/bias Gemm forms have NO device lowering today (correctly fail-closed, proven via LMM-CHECK-OPS). Fix: extend the matmul epilogue emitter with binary/affine epilogue ops - OP-SCALE (acc*alpha, scalar constant) and OP-BIAS (acc+bias[col], note LMM-BIAS already does exactly this for LINEAR - likely reuse) - plus their cg-activation-style emit words, capability regression, and a device golden for an alpha<>1 transB Gemm (extends deploy-composed-device). Acceptance: alpha/bias composed forms lower + run device-correct vs reference; pure-unary epilogue behavior unchanged; the fail-closed characterization test flips to a positive. FENCED: maki/lower-mm.f is in sol's region-lower territory (habu-v2-r3-type-144b5fa2) + the makipools remainder - coordinate/hold until those release. Files: maki/lower-mm.f, lib/ptx cg emitters if needed, deploy-composed-device(+test), maki tests. Verify: lower-mm(-device)-test, maki/test.f, on-device goldens. Ownership: maki matmul lowering.
