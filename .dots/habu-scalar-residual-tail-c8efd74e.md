---
title: Scalar-residual tail for v4 (general n, correctness)
status: open
priority: 3
issue-type: task
created-at: "2026-06-27T15:19:39.082623+02:00"
---

The checked v4 tile path is landed, but it assumes n%4==0. For general n add a masked scalar epilogue: threads whose base_elem+lane < n store via scalar st, else skip; or a separate tail kernel for the last n%4 elements. Makes the vectorized kernel correct for any n (removes the divisible-by-4 precondition / soundness boundary). VERIFY: correct for n=4,5,7,1000003.
