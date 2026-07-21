---
title: "Infer: KV-cache quantization"
status: open
priority: 3
issue-type: task
created-at: "2026-07-21T16:45:21.609887+02:00"
blocks:
  - habu-infer-nvfp4-quantized-ea42f1ae
---

Plan-of-record M9, separate from weight quantization: bf16/fp16 reference -> FP8 KV candidate -> lower-bit only with model-specific quality evidence. Gates: long-context quality degradation measured; attention-kernel bandwidth improves in the regimes where KV reads dominate; capacity math includes scale/metadata overhead; explicit user-selected quality profile.
