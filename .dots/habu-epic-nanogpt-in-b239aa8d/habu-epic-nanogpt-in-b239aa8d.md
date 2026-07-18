---
title: "EPIC: nanoGPT in Habu"
status: open
priority: 1
issue-type: task
created-at: "2026-07-18T15:04:37.707301+02:00"
---

North star (2). Rebuild Karpathy nanoGPT in Habu: inventory existing maki pieces (attention.f, matmul, softmax, layernorm, optim/loss families, autograd, tokenizer gap), gap list -> GPT-2-small-shaped composition (token+pos embeddings via gather/idxctx, N blocks, LM head, cross-entropy, AdamW, tiny-shakespeare training loop) as CPU goldens first, then PROMOTE through the compute campaign to train on GB10. LOAD-BEARING (Joel): the model is AUTHORED as SPEC: lines from the start — the SPEC: chain (A1 -> TENSOR: -> SPEC:) is critical path and dispatches first on the checker lane; early goldens may be prototyped in current idiom only to de-risk numerics, then rewritten as SPEC: before composition. Sub-dots minted after the inventory.
