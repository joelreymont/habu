---
title: V2 reference framework environments
status: open
priority: 1
issue-type: task
created-at: "2026-07-11T12:25:27.814087+02:00"
---

Provision version-pinned isolated PyTorch, torch.compile/Inductor, Triton, ONNX Runtime, TensorRT, CUDA-library, and profiler reference environments as specified by MODEL-CAD-V2-PLAN.md:1774-1793. Use official device-supported installation paths, record lock/environment/device digests, and add scalar, tensor, shared-model, compile, and profiler smokes. Mac is orchestration only; Spark and Orin rows are separate. No framework becomes a Habu semantic owner. Acceptance: supported rows pass and unsupported rows produce explicit facts. This dot owns the requested installations.
