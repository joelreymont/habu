---
title: "Infer: fused decode attention kernel"
status: open
priority: 1
issue-type: task
created-at: "2026-07-21T15:58:14.142912+02:00"
blocks:
  - habu-infer-decode-paged-66b6a16d
---

This is the decode-attention campaign record. Do not dispatch it as implementation work. Its leaves own the checked geometry, online-softmax oracle, contiguous kernel, paged iterator, transfer candidates, measured transfer selection, and real-model parity. The campaign closes when the paged real-model parity leaf lands with the selected transfer path.
