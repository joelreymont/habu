---
title: "CAD: weight layout owned at PROMOTE time"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T19:28:43.753314+02:00"
---

CAD-PLAN 8.1 lever 5. The promoted artifact owns its weight layout: pre-transpose / pre-swizzle weights into the layout the winning schedule wants (e.g. column-major or MMA-fragment-swizzled W for the gemm family) at PROMOTE, so kernels never pay transposition at run time. Needs: a layout field in the schedule selection, a PROMOTE-time transform pass over the artifact weights (host-side, checked, golden-verified roundtrip), and the loader honoring it. Depends: slice-3 matmul lowering + cad-6-tune (the schedule picks the layout). Blocks: end-to-end latency parity work.
