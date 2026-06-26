---
title: "PTX: local type inference (infer bodies, annotate the edge)"
status: open
priority: 2
issue-type: task
created-at: "2026-06-26T23:18:42.431173+02:00"
blocks:
  - habu-ptx-m4-tile-6a825f56
---

docs/inference.md. Drop required annotations on intermediate {: x :} bindings (infer from top-of-stack); confine annotation to the kernel signature + trusted constructors (MK-SPAN/MK-MATRIX). Inference threads extent/mask/space tokens so relational proofs carry without restatement. Recursion/branches keep declared effects; extent tokens stay nominal; add a :type/show-inferred form {: x:? :}. Cross-cutting checker surface; usable once tile+locals exist (M4).
