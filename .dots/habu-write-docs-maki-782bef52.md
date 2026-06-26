---
title: Write docs/maki/autograd.md design (tensor-op orchestration)
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T00:01:19.308528+02:00"
---

GATE. Design the maki-LEVEL autograd ORCHESTRATION, distinct from the Habu kernel transform (docs/autograd.md). Specify: the tensor-op VJP rule table (Linear/MatMul/elementwise/reductions) that LOWERS ONTO the Habu primitive VJP: table (state the C-vs-D seam), the user-facing define-forward-get-checked-backward API, and optimizer hand-off.
- Files: new docs/maki/autograd.md.
- Verify: tensor-op table present; the seam to Habu primitive VJP: is explicit; user API defined.
- Dep: none. Gates maki autograd-orchestration impl.
