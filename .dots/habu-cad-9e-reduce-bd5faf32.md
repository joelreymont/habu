---
title: "CAD 9e: reduce/scatter backward ops"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T07:27:49.145875+02:00"
---

cad-9 follow-up (worker boundary, gates the training flagship): bias/scale/linear param-gradients need reduce backward ops (bias-grad = row-reduce sum of the cotangent; scale-grad = full-reduce dot) and slice/gather adjoints need scatter ops (slice -> pad-scatter, gather -> scatter-add). Add op-kinds OP-ROWSUM-BWD / OP-FULLSUM-BWD / OP-PAD-SCATTER / OP-SCATTER-ADD with registry rows + host buffer references + adjoint-table rows flipping ADJ-SUP? true for scale/bias/linear/slice/gather; backward.f emitters; gradcheck host-exec extension for the reduce class where a scalar sample is NOT exact (needs small-buffer eval - extend GC-APPLY to row granularity for these). Tests: linear MLP model gradchecks end-to-end on host; slice/gather adjoint numeric checks. Depends: cad-9 (landed). Blocks: habu-maki-from-scratch (MLP needs linear adjoint).
