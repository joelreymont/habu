---
title: "EPIC: nanoGPT in Habu"
status: open
priority: 1
issue-type: task
created-at: "2026-07-18T15:04:37.707301+02:00"
---

North star (2). Rebuild Karpathy nanoGPT in Habu: inventory existing maki pieces (attention.f, matmul, softmax, layernorm, optim/loss families, autograd, tokenizer gap), gap list -> GPT-2-small-shaped composition (token+pos embeddings via gather/idxctx, N blocks, LM head, cross-entropy, AdamW, tiny-shakespeare training loop) as CPU goldens first, then PROMOTE through the compute campaign to train on GB10. Authoring surface upgrades (TENSOR:/SPEC:) adopted as they land — do not gate on them. Sub-dots minted after the inventory.
