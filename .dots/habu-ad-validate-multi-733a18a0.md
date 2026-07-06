---
title: "AD: validate multi-output cotangent threading (data-flow)"
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T08:06:44.243271+02:00"
---

Gap #7. The reverse pass is a SYNTACTIC token transform; binary nonlinear ops (*./B-) produce two cotangents via stack juggling (DUP/SWAP) in their expansion, but correctness of the cotangent routing for ARBITRARY point-free pipelines is not validated against the actual data-flow graph - only the demonstrated straight-line cases. Build a data-flow model (or prove the point-free stack discipline guarantees correct routing) and add gradcheck over multi-op pipelines mixing binary nonlinear ops.
- Files: lib/ptx/ad.f, lib/ptx/ad-test.f, the device gradcheck.
- Verify: a pipeline with *., B-, DUP fan-out gradchecks numerically on device.
- Dep: ad-reverse (closed 2026-07-06; former blocks: entry dropped).

## Audit refresh (2026-07-06, head 1eb3b5d3)

The core premise ("cotangent routing not validated against the actual data-flow
graph") is stale: lib/ptx/ad-dag.f IS a value-numbered data-flow DAG with DUP
fan-out and `+.` cotangent accumulation (lib/ptx/ad-dag-test.f rc 0), and the
generated backward device-gradchecks on Orin (50fb46683627). The work landed in
ad-dag.f, not the ad.f files named above. Remaining scope: broaden the DAG op
set beyond the softmax primitives (e.g. `*.`) and gradcheck multi-op pipelines
mixing binary nonlinear ops per the original verify.
