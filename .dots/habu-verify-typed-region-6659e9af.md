---
title: Verify typed region lowering on Orin
status: open
priority: 2
issue-type: task
created-at: "2026-07-13T13:46:22.741230+02:00"
---

Full context: habu-v2-r3-type-144b5fa2 has reviewed typed region lowering changes and all off-device owning gates green, but maki/device-smoke.f reports libcuda.so.1 unavailable on the macOS integration host. Acceptance: on the Orin, rebase the exact landed region-lowering tree; run maki/lower-model-device-test.f and every touched lower-ew/red/mm/mv/model CUDA golden; prove emitted PTX remains byte-exact and device outputs element-exact; rerun maki/test.f, ptx-stdlib, host/filemap/dot lints, and the full native gate; attach command output and close habu-v2-r3-type-144b5fa2 only after green. Files: device proof artifacts/status only unless a failure requires a root-cause implementation fix. Availability: Orin expected 2026-07-15.
