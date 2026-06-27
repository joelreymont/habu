---
title: "Eval matrix: Triton comparison arm"
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T12:11:42.952354+02:00"
---

The thesis is Habu-PTX vs Triton (a better LLM target). The Triton side - author the same kernel tasks in Triton, measure pass@k / repair-rounds / tokens-to-green under the same loop - needs the Triton toolchain + a model authoring Triton. Produce the comparative columns so the thesis can be validated/refuted. External-tool dependency (Triton + model).
