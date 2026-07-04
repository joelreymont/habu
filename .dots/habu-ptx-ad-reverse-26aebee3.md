---
title: "PTX AD: reverse-mode transform"
status: open
priority: 3
issue-type: task
created-at: "2026-06-26T23:18:42.445886+02:00"
blocks:
  - habu-ptx-ad-device-2b511851
---

docs/autograd.md. Reverse-mode AD as a syntactic reversal of the concatenative IR (no runtime tape). VJP: paired adjoint words (DUP<->+., BLOCK-SUM<->BROADCAST, LOAD<->STORE, EXP., *., B-, SCALE, FMA, BLOCK-MAX scatter); reverse pass + algebraic-simplify + save-vs-recompute; scatter-add (red.global.add) for accumulating adjoints; SOFTMAX-ROWS-BWD checked. Needs BROADCAST (the implicit op in B-/B/) plus the device finite-difference gradcheck hard gate before any verified-gradient claim.
