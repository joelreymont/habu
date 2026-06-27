---
title: "AD: validate multi-output cotangent threading (data-flow)"
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T08:06:44.243271+02:00"
blocks:
  - habu-ptx-ad-reverse-26aebee3
---

Gap #7. The reverse pass is a SYNTACTIC token transform; binary nonlinear ops (*./B-) produce two cotangents via stack juggling (DUP/SWAP) in their expansion, but correctness of the cotangent routing for ARBITRARY point-free pipelines is not validated against the actual data-flow graph - only the demonstrated straight-line cases. Build a data-flow model (or prove the point-free stack discipline guarantees correct routing) and add gradcheck over multi-op pipelines mixing binary nonlinear ops.
- Files: lib/ptx-ad.f, lib/ptx-ad-test.f, the device gradcheck.
- Verify: a pipeline with *., B-, DUP fan-out gradchecks numerically on device.
- Dep: ad-reverse.
