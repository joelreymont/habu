---
title: Move device suites out of maki test
status: active
priority: 2
issue-type: task
created-at: "2026-08-05T11:22:30.769998+02:00"
---

Successor to habu-attr-the-kv-22e4e99f. maki/test.f is not host-independent: it aborts at the first device-required suite, so 'maki green on the Mac' is unreachable by fixing any one file. Proven device-required: maki/infer/kv-cache-test.f (dies 74 with named cause since 41f344a1) and maki/gpu-buffer-test.f (same -5002 next in line). Suspected, unproven: maki/lower/model-test.f, maki/onnx/deploy-test.f, maki/infer/gpt2-model-test.f, maki/eval/device-fault-test.f, maki/device-smoke.f — each references the driver without installing MKD fakes; classify each. Decision (per the tree's own convention, not consensus): the repo already keeps the *-device-test.f family out of maki/test.f and runs it explicitly on a device host (docs/ablation.md). Move the proven device suites into that family the same way, THE WHOLE FAMILY AT ONCE (moving one just fails one suite later), so maki/test.f becomes the host-independent gate and device coverage runs in the device gate on spark/Orin. The rejected alternative — a host-backed memory double for the MKD seam — costs the real-driver exercise the suites exist for and ~90 lines of new machinery; do not build it without a measured need. Acceptance: bin/hb --load maki/test.f green on a driverless Mac; every moved suite green via the device gate on spark; docs/ablation.md names the new members; no suite lost from coverage (count them before and after).

Claim: agent=device-suites workspace=.jj-ws/habu-move-device-suites-b4fff868
