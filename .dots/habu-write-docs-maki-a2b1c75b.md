---
title: Write docs/maki/tensors.md design
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T00:01:19.294433+02:00"
---

GATE for maki tensor impl. Design the tensor type over raw tiles to inference.md/autograd.md depth: shape/rank, dtype set (f32/f16/bf16/u32/i32 per sm_87, no fp8), layout, broadcasting rules, the trusted constructor boundary (how a tensor mints span/matrix + extent tokens), and how tensor shapes map to PTX extent tokens. One concern per file.
- Files: new docs/maki/tensors.md.
- Verify: covers shape/dtype/layout/broadcast/constructor; names the M4 tile primitives it lowers onto.
- Dep: none (write now). Gates maki tensor-types impl.
