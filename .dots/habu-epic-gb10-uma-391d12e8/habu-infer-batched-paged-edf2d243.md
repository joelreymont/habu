---
title: "Infer: batched paged decode"
status: open
priority: 2
issue-type: task
created-at: "2026-07-21T16:44:54.214923+02:00"
blocks:
  - habu-infer-fused-decode-77f72ca7
---

Plan-of-record M3 stage C: bounded small-batch decode over per-sequence block tables - different sequence lengths, different table lengths, completed-sequence masking, NO host-side per-head launch loop. Contiguous-vs-paged agreement and real-model greedy parity extend to the batched case; context-regime benchmarks per the kernel-family contract.
