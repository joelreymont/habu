---
title: Write docs/maki/eval.md design
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T00:01:19.330870+02:00"
---

GATE. Design the TWO distinct evals (do not conflate): (i) the kernel-authoring matrix (ptx-sketch.md LLM-experiment: kernels vector-add/row-reduce/argmax/softmax-row; arms Habu-PTX vs raw Triton; metrics pass@k/repair-rounds/tokens-to-green/GB-per-s) - already specced upstream; (ii) a maki MODEL train/eval (datasets, model, accuracy metric) - new. The raw-Triton + LLM-driver arms are Python; AGENTS.md Habu-Only forbids new Python and host-lint rejects .py - so the orchestrator is Habu-native and any external runner is a NAMED tested host-glue boundary tracked by a retire-it dot, not loose .py under maki/.
- Files: new docs/maki/eval.md.
- Verify: both evals specified; the no-Python host-glue boundary defined; NO better-target claim until the matrix produces data.
- Dep: none. Gates the eval-matrix impl.
