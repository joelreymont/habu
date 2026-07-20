---
title: Raise MIR node-table cap for multi-block models
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-20T15:10:30.015838+02:00\""
---

Capacity wall proven at the GPT-2 block landing (8207fd54): the differentiable 2-block stack dies E-MIR-CAP (-5055) at node 128 during BW-BUILD - maki/model-ir.f:130's node-table cap. Measured accounting: ONE attention-bearing block's full forward+backward IR is 74 nodes (18 fwd + 56 adjoint); two blocks' adjoints exceed 128, and nanoGPT wants Nx (12 blocks for GPT-2 small = ~900 nodes by linear extrapolation). Mechanically proven: ' NX-BUILD E-MIR-CAP TTHROWS committed in gptblock-attn-test.f:361-388, with the 2-block FORWARD composing fine (ref ring at most 2 outstanding of 7 - not the binding resource). Fix the same shape as REQUIRE-MAX/TR-CAP raises: measure per-node row size, raise the cap with headroom for the Nx target (>=1024 suggests itself for 12 blocks; justify from the row size x DATA budget), keep the fail-closed named die, red-first cap+1 regression, retire the TTHROWS wall test in favor of a working 2-block differentiable lock. Measure DATA/CODELEN impact and update size rows same-commit if image-resident. Territory: maki/model-ir.f + the gptblock-attn wall test + a capacity regression.

Claim: agent=mircap workspace=.jj-ws/fable-mircap machine=spark (owns maki/model-ir.f + maki/examples/nanogpt/gptblock-attn-test.f wall test + capacity regression)
