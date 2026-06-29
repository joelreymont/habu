---
title: Scalar-residual tail for v4 (general n, correctness)
status: open
priority: 3
issue-type: task
created-at: "2026-06-27T15:19:39.082623+02:00"
---

The checked v4 tile path is landed, but it assumes n%4==0. For general n add a masked scalar epilogue: threads whose base_elem+lane < n store via scalar st, else skip; or a separate tail kernel for the last n%4 elements. Makes the vectorized kernel correct for any n (removes the divisible-by-4 precondition / soundness boundary). VERIFY: correct for n=4,5,7,1000003.

Mac progress: `lib/ptx/cg-vec.f` now keeps `GRID-CTX-V4` as a base-element
context, emits `ld/st.global.v4.f32` only for full vectors, and emits predicated
scalar load/store lanes for residual vectors. Static/codegen tests prove the
fast path and tail path are present. Remaining proof before close: run the v4
CUDA device golden on zed for n=4,5,7,1000003.
