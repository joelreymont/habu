---
title: "Infer: GPT-2 124M real-weights forward"
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-21T15:57:56.745951+02:00\""
blocks:
  - habu-infer-safetensors-loader-0b58e06a
---

The first REAL pretrained model through Habu: load HF GPT-2 124M from safetensors (the loader dot's artifact + Conv1D orientation note), build the forward over the landed machinery (LayerNorm affine, GELU, the composition path - the nanoGPT track already proves the architecture trained-from-scratch; the new bit is real weights at real scale: 12 layers, 768 dim, 12 heads, 50257 vocab), and prove logits parity against a committed reference (transformers or hand-rolled torch over the same safetensors in the ml venv, f64 reference, exact ids on greedy continuation of fixed prompts - the gptblock-torch-ref pattern at real scale). Host forward first (correctness before speed); attention via the existing composed path. Deliverable includes the measured host tokens/sec as the baseline the device path must beat. This is the reference generator for the decode-attention kernel dot.

Claim: agent=gpt2fwd workspace=.jj-ws/fable-gpt2fwd machine=spark (owns the GPT-2 124M real-weights host forward + reference fixture)
