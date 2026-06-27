---
title: "Maki: optimizers (SGD/Adam) + losses"
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T00:01:50.979217+02:00"
---

D. Implement SGD/Adam + losses per docs/maki/optim.md, with the param/grad update contract from the autograd orchestration. Strictly typed Habu, T{ }T per word.
- Files: maki/optim.f, maki/loss.f (one concern per file).
- Verify: a one-step SGD/Adam update on a tiny param tensor matches a CPU golden; loss + its grad gradchecks.
- Dep: docs/maki/optim.md + maki tensor types + maki autograd orchestration.
