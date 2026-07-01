---
title: "Autograd: end-to-end model-grad parity vs CPU/PyTorch reference"
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T23:00:02.531465+02:00"
blocks:
  - habu-autograd-transformer-block-e2d41299
  - habu-maki-lower-tensor-e6bbca3d
---

File: PLAN.md:432. Gap: individual VJPs do not prove the Maki graph can run a
whole model backward with device-resident buffers and match the CPU reference.
Fix: add an end-to-end Maki gradient parity harness for the transformer-block
path, using the generic VJP table and Maki device tensor/runtime rather than a
capstone-specific shortcut. Verify: per-step gradients match CPU within
dtype-specific tolerance, device finite-difference checks run for lowered PTX
ops, and the harness feeds the later nanoGPT capstone without depending on it.
