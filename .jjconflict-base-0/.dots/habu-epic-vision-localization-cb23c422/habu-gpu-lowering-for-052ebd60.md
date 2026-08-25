---
title: GPU lowering for vision ops
status: open
priority: 2
issue-type: task
created-at: "2026-07-20T11:23:15.770557+02:00"
blocks:
  - habu-patchify-op-with-a6dd25f1
---

Lower the new vision ops through the existing PTX path: patchify as a strided-gather kernel (or an im2col-style copy feeding the existing GEMM), mean-pool as a reduction, the losses as elementwise kernels with their adjoints, reusing the autodiff DAG and kernel-manifest machinery. Parity fixtures against the host reference at several shapes, and per-op precision hooks per habu-wire-per-op-d34a0b66. Device legs run on spark's lanes; keep host-side compilation and PTX text assertions runnable on the Mac.
