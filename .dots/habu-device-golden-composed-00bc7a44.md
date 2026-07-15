---
title: Device-golden composed Gemm forms (transB/alpha)
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-15T18:07:41.766003+02:00\""
---

Discovered by the onnxdev lane 2026-07-15: maki/onnx/import.f lowers non-default Gemm (transB=1, alpha<>1, beta<>1) by COMPOSITION (inserted TRANSPOSE/SCALE movement+elementwise nodes around OP-LINEAR), and host-side import tests cover it, but no composed form has a DEVICE golden - the whole-model device path with an inserted movement node between import and matmul regions is untested on device (maki/onnx/deploy-device-test.f pins only the default affine Gemm -> GEMM-SIMPLE -> pure 2-region LINEAR shape). Fix: commit an ort-ref fixture for a transB (and alpha<>1) Gemm MLP (onnxruntime reference values committed like ort-ref-data.f), extend the deploy-device harness to that model, prove the movement-node region lowers and runs on-device, golden vs ort under the composed tolerance discipline, plus a corruption probe. Depends on the movement region class being device-emittable for the inserted TRANSPOSE (verify; if not, that gap becomes the first sub-item). Files: maki/onnx/ort-ref-data sibling fixture, deploy-device(+test) or a sibling test, possibly maki/lower-move.f evidence. Verify: on-device golden PASS + probe FAIL, off-device SKIP honest, maki/test.f. Ownership: maki onnx device coverage.

Claim: agent=gemmdev workspace=.jj-ws/fable-gemmdev
