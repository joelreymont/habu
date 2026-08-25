---
title: Complete batched position embedding
status: open
priority: 1
issue-type: task
blocks:
  - habu-lowering-hash-unified-586f7881
created-at: "2026-07-19T23:28:19.952884+02:00"
---

The closed learned-positional-embedding claim proves only B=1 buffer behavior and a different pre-shaped model. maki/embedding.f WPE-SLICE copies the first T x C rows and TOKPOS-EMBED explicitly documents B=1; neither is consumed by MODEL/AD. maki/pos-embed-test.f instead declares wpe:6x3 beside ids:6x1 and runs GATHER ADD, so its positional parameter has B*T independent rows. GPT-2 owns one MaxT x C table shared by every batch: for B>1, position j must reuse wpe[j] in each batch item and its gradient must sum all B contributions. The current composition cannot express or test that contract, yet the feature dot is closed as token+position composition. Add a real model/IR operation or SPEC chain accepting wte, token ids, one MaxT x C wpe table and checked {batch,time,channels,max-time}; lower shared position lookup/broadcast and its adjoint so gradients accumulate by position across batches. Remove the B=1-only public prototype once the real path owns it. Add B=2 with distinct tokens but identical position rows, T=1/MaxT/exceed, repeated tokens, exact forward, finite differences for every wte/wpe parameter, gradient multiplicity B, training, host/device lowering parity, and proof the GPT-2 block consumes this operation. Correct the closed feature prose so the buffer golden is not reported as full composition. Use STRUCTURE batch-sequence-shape after unified lowering. Files: maki embedding/model/AD/lowering tests and historical feature dot. Block on habu-lowering-hash-unified-586f7881; API bounds and packaging remain separate owners.
