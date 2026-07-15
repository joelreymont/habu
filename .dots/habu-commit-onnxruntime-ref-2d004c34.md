---
title: Commit onnxruntime reference for composed Gemm forms
status: open
priority: 2
issue-type: task
created-at: "2026-07-15T19:36:42.250241+02:00"
---

Residual from habu-device-golden-composed-00bc7a44 (2026-07-15): the composed-Gemm (transB) device golden uses the HOST-executor oracle (validated ==onnxruntime within 1e-5 on the affine ort-ref fixture) because no committed onnxruntime reference exists for a composed form and no machine in the fleet may have onnxruntime installed by an agent. USER-GATED (E2): producing the reference requires the user to run onnxruntime 1.27.0 once (same flow that produced maki/onnx/ort-ref-data.f) against the committed composed fixture bytes (maki/onnx/composed-ref-data.f CRF model - encode.f DSL, no blob) and hand back y values. Then: commit them as the composed ort reference, switch deploy-composed-device-test.f's reference from host-oracle to committed-ort (keeping the host leg as free coverage), re-run the device golden on zed. Acceptance: composed golden compares device vs committed ort values; provenance header updated; probe still FAILS. Files: maki/onnx/composed-ref-data.f, deploy-composed-device(-test).f. Ownership: maki onnx device evidence; blocked on user-supplied onnxruntime output.
