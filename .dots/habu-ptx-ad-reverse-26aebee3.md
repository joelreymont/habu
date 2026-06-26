---
title: "PTX AD: reverse-mode autograd (verified gradients)"
status: open
priority: 3
issue-type: task
created-at: "2026-06-26T23:18:42.445886+02:00"
blocks:
  - habu-ptx-m6-collectives-12cf0e2d
---

docs/autograd.md. Reverse-mode AD as a syntactic reversal of the concatenative IR (no runtime tape). VJP: paired adjoint words (DUP<->+., BLOCK-SUM<->BROADCAST, LOAD<->STORE, EXP., *., B-, SCALE, FMA, BLOCK-MAX scatter); reverse pass + algebraic-simplify + save-vs-recompute; scatter-add (red.global.add) for accumulating adjoints; SOFTMAX-ROWS-BWD checked. Needs BROADCAST (the implicit op in B-/B/). Buildable after M6; verified gradients are the strongest form of the ptx.md LLM-target claim and feed M11.
