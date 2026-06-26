---
title: Write docs/maki/optim.md design
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T00:01:19.314336+02:00"
---

GATE. Design SGD/Adam optimizers + loss set + the parameter/gradient update contract (how params and grads flow from the autograd orchestration into an update step).
- Files: new docs/maki/optim.md.
- Verify: SGD + Adam + at least one loss specified with the update contract.
- Dep: none. Gates maki optim impl.
