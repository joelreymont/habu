---
title: Automatic op-fusion pass (register-resident, the bandwidth win)
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T15:43:15.234314+02:00"
blocks:
  - habu-make-ptx-device-c0eb12a3
  - habu-add-ptx-planner-30b93e8c
  - habu-ptx-ir-opt-b90390f0
---

File: PLAN.md:322. Gap: current fusion is effectively an elementwise
string-concat proof around Maki/ONNX shapes and fixed temp paths, not a generic
PTX region/fusibility layer. Fix: build PTX-owned region nodes, same-shape
elementwise barriers, alias/layout/dtype/register-pressure split reasons, and
private-temp/profiled fused-vs-unfused drivers; keep `maki/fusion.f` as a MAKI
adapter. Verify: fused and unfused device rows match values, profile rows show
reduced global bytes, failures throw named errors with split reasons, and no
public fusion path exposes raw PTX strings or `/tmp` drivers.
