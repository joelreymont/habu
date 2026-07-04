---
title: "Maki: retire hardcoded SAXPY dependencies"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T11:19:47.377822+02:00"
---

SAXPY must become ONE committed fixture, not maki's structural device kernel. Inventory (rg -l -i saxpy maki/): RUNTIME PATH (the real dependency, must go): maki/gpu.f loads /tmp/saxpy.cubin + G-SGD-STEP lowers the tensor SGD step onto the SAXPY kernel (a=-lr,x=grad,y=weight); maki/gpu-train.f builds the GPU training loop on that lowering; gpu-sgd-test.f/gpu-train-test.f pin it; maki/fusion.f + maki/onnx.f reference saxpy kernels in lowering/mapping paths (audit each site). Replacement: the cad device leg (habu-maki-lower-tensor) emits kernels FROM the model IR / fusion plan; the optimizer step becomes an emitted elementwise kernel from the op set (w -= lr*g as OP-SCALE/OP-ADD or one fused elementwise), selected by the schedule machinery - not a named special case. gpu.f's cubin load moves to PTXTC self-emit (subsumes dot habu-maki-gpu-f - close it when this lands). EVAL HARNESS (keep as fixtures, de-structure): eval-fixture/author/repair/compare may keep SAXPY as committed eval TASKS, but maki/eval-device.f's device gate golden (0x40C00000) hardcodes SAXPY as THE device check - generalize the device gate to grade whatever kernel the task under eval emits, with SAXPY just task #1. Docs: maki/README.md + STATUS.md updated to describe the general lowering. Ordering: needs habu-maki-lower-tensor's first slice (emit an elementwise kernel from a fusion region); do the gpu.f/gpu-train.f retirement in the same campaign so the training flagship's GPU leg lands on the general path, never deepening the SAXPY weld. Depends: habu-maki-lower-tensor, lib/ptx module emission (minion-ptxmod lane). Blocks: GPU training step milestone.
