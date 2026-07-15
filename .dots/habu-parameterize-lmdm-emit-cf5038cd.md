---
title: Parameterize LMDM emit harness (require+source)
status: open
priority: 2
issue-type: task
created-at: "2026-07-15T18:07:41.762131+02:00"
---

Discovered by the onnxdev lane 2026-07-15 closing habu-maki-onnx-import-f37c9458: maki/onnx/deploy-device.f duplicates the whole-model per-region cubin build (ODV-EMIT/ODV-ASSEMBLE-REGION/ODV-BUILD-CUBINS mirror maki/lower-model-device.f LMDM-EMIT/...) because LMDM-EMIT hardcodes the MODEL:-source capture + class-only require in the spawned child's driver. Fix: parameterize the LMDM harness on (reqa require-line, ma model-source-line) so both the MODEL:-capture path and the ONNX:IMPORT path share ONE emit/assemble/build implementation; deploy-device.f keeps only its import-specific bridge words and tolerance. Acceptance: ODV-EMIT/ODV-ASSEMBLE-REGION/ODV-BUILD-CUBINS deleted, deploy-device-test.f + lower-model-mlp-device-test.f both green on-device via the shared harness, off-device SKIP + host legs unchanged. Files: maki/lower-model-device.f, maki/onnx/deploy-device.f(+test). COORDINATE: maki/lower-*.f is fenced by sol's region-lower claim (habu-v2-r3-type-144b5fa2) - check claims at dispatch. Verify: maki/test.f, on-device run of both tests, suite-coverage-lint. Ownership: maki lowering harness.
