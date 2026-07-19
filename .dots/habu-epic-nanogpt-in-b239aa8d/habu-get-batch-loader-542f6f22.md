---
title: get-batch loader into B*T-row buffer
status: closed
priority: 2
issue-type: task
created-at: "\"2026-07-18T17:36:22.590247+02:00\""
closed-at: "2026-07-19T08:26:43.885286+02:00"
blocks:
  - habu-tiny-shakespeare-char-125d9684
---

Shape get-batch output to the decided layout: B sequence windows of length T written as contiguous row blocks (rows = B*T, B outermost) into the embedding input (EMB-GATHER embedding.f:23) and the cross-entropy target vector; emits the segment attribute value T for the segment attention op. Fail-closed: a batch exceeding MIR-CAP = 128 nodes (model-ir.f:100) or the 32768-cell executor arena (executor.f:73,75) throws, never truncates. Full contract: docs/batch-sequence-design.md section 5 BTC-4.
