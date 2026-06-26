---
title: "Maki: tensor/array types over tiles"
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T00:01:50.956694+02:00"
blocks:
  - habu-write-docs-maki-a2b1c75b
---

D. Implement the maki tensor type per docs/maki/tensors.md: shapes, dtypes, layouts, broadcasting, the trusted constructor that mints span/matrix + extent tokens. Lowers onto M4 tiles. Strictly typed Habu (CHECKED:), small factored words, T{ }T per word.
- Files: maki/tensor.f (+ split by concern if needed).
- Verify: tensor construct/reshape/broadcast checks clean; shape mismatch rejected; lowers to M4 tile ops.
- Dep: docs/maki/tensors.md (habu-write-docs-maki-a2b1c75b) + M4 (habu-ptx-m4-tile-6a825f56) + maki scaffold.
