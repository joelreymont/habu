---
title: Integrate AdamW in Maki device graph
status: open
priority: 2
issue-type: task
created-at: "2026-07-01T22:30:40.045947+02:00"
blocks:
  - habu-maki-lower-tensor-e6bbca3d
  - habu-autograd-transformer-block-e2d41299
  - habu-add-logits-domain-a1489686
---

File: PLAN.md:484; cause: maki Adam scalar rules are closed, but the GPT capstone still needs decoupled AdamW over parameter/state tables in the device graph with parity and loss-decrease proof; fix: add generic Maki optimizer graph op for AdamW, parameter/state layout, CPU/device one-step parity, lowering through generic PTX device runtime, and capstone loss-decrease integration; deps: Maki device lowering, transformer VJPs, CE/loss lowering; verification: maki optimizer/device tests plus Orin capstone gate prove one AdamW step and short loss decrease without introducing optimizer APIs under maki/gpt*.
