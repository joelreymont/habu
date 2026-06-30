---
title: Render PTX IR value graphs
status: closed
priority: 2
issue-type: task
created-at: "2026-06-30T09:53:43.868909+02:00"
closed-at: "2026-06-30T09:56:44.804484+02:00"
close-reason: "Implemented checked PTXIR RPN token renderer in lib/ptx/ir.f with fixtures for peephole render, folded constants, symbolic inputs, and softmax-backward closed form. Proof: focused PTX static suite ok; typed-local/trust/dot/filemap lints ok; full local suite 24772ms internal / 26.885s wall."
---

Local slice for habu-ptx-ir-opt-b90390f0. Problem: PTXIR has fold/CSE/DCE and softmax-bwd value graph proof, but no lowering/render surface for consumers. Fix: add checked RPN token renderer in lib/ptx/ir.f and fixtures in lib/ptx/ir-test.f proving folded constants, peepholes, and softmax-bwd closed form render as stable token text. Verify: focused PTX static suite, typed-local diff lint, trust/dot/filemap lints, full local test suite. No zed/device work.
