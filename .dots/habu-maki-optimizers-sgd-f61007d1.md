---
title: "Maki: optimizers (SGD/Adam) + losses"
status: closed
priority: 2
issue-type: task
created-at: "2026-06-27T00:01:50.979217+02:00"
closed-at: "2026-07-01T18:05:32+02:00"
---

D. Implement SGD/Adam + losses per docs/maki/optim.md, with the param/grad update contract from the autograd orchestration. Strictly typed Habu, T{ }T per word.
- Files: maki/optim.f, maki/loss.f (one concern per file).
- Verify: a one-step SGD/Adam update on a tiny param tensor matches a CPU golden; loss + its grad gradchecks.
- Dep: docs/maki/optim.md + maki tensor types + maki autograd orchestration.

Closed with scalar SGD/momentum/decay/Adam, scalar MSE/L1, tensor MSE gradient
finite-difference coverage, and tensor Adam one-step CPU golden coverage in
`maki/optim-tensor-test.f`.
