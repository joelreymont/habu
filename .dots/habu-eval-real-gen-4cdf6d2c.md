---
title: "Eval: real generation-token count + softmax/collective authoring tasks"
status: open
priority: 3
issue-type: task
created-at: "2026-06-27T12:11:42.961234+02:00"
---

tokens-to-green (maki/eval-repair.f) is a whitespace source-token proxy, not model generation tokens; wire a tokenizer/model-token count. Also the autograder fixtures are SAXPY-only - add softmax-rows and collective authoring trajectories (the kernels are device-validated; add their pass@k / repair fixtures).
