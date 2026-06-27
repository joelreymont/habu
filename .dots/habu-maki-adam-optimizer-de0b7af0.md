---
title: "Maki: Adam optimizer"
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T08:06:44.256951+02:00"
blocks:
  - habu-maki-optimizers-sgd-f61007d1
---

Gap #12. Only SGD family is built (maki/optim.f: SGD, SGD-MOM, weight-decay). Add Adam: m = b1*m + (1-b1)*g ; v = b2*v + (1-b2)*g^2 ; bias-correct (needs b1^t, b2^t - add an fpow or iterate) ; w -= lr*mhat/(sqrt(vhat)+eps). Float rules + tensor-scale version (over arrays).
- Files: maki/optim.f, maki/optim-test.f.
- Verify: one Adam step matches a CPU golden; convergence on the linear-fit demo.
- Dep: maki optimizers (SGD done).
