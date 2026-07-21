---
title: Wire model-device ptxas arch probe
status: open
priority: 2
issue-type: task
created-at: "2026-07-21T07:41:44.423564+02:00"
blocks:
  - habu-fix-softmax-gradcheck-a2020d85
---

Loose end from the maki CUDA migration (stack cb1e4cae), pre-existing on base: maki/lower/model-device.f LMDM-PTXAS never calls PTXTC:TC-ARCH! before assembling region cubins, so model-mlp-device-test.f throws E-PTXTC-ARCH on any box whose arch differs from the default - same class as the softmax-gradcheck gap (habu-fix-softmax-gradcheck-a2020d85) and same fix idiom (the ATGT probe, maki/gpu.f ASSEMBLE-PTX precedent). Fix both this file and fold in the softmax dot's audit so every device tool probes; standalone runs on GB10 prove it.
