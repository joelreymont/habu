---
title: External deterministic golden for the block
status: closed
priority: 1
issue-type: task
created-at: "2026-07-20T22:41:31.077467+02:00"
closed-at: "2026-07-21T00:15:56.137401+02:00"
close-reason: "Landed (stack tip 24c19014): external deterministic PyTorch golden for the full attention-bearing GPT-2 block, reconciled onto the TIED composition after the tie landed first. Committed f64 CPU torch-2.9.1 fixture (adam-torch-ref pattern: inline provenance, fail-closed fill, named E-codes) holding the tied forward logits and the 12-step pre-update loss trace; section (G) reuses the tie lane's machinery with no second backward build. Replica ties identically (one wte tensor, logits = F @ wte.T, autograd sums both paths into one Adam moment pair) and reproduces the internal tied milli locks 1665/27 EXACTLY. Measured floors: forward rel-L2 2.04e-9, trace max rel 3.81e-7; tolerances derived at ~49x/~13x floor, >=200x below any structural defect. Load-bearing proof: perturbed logit and loss each go red, restored green, and the internal and external goldens pass in the same run. Both sides are f64 - the task premise 'composition runs f32' was wrong, floor is the transcendental-polynomial gap. Untied path remains internally locked only (single golden retargeted to tied by decision); external grounding for it would be a separate fixture if ever wanted"
---

The block forward golden compares the executor to GBR-FWD, an internal hand-written reference built to mirror the engine's contraction order cell-for-cell - self-consistent, not external. Produce an external deterministic golden (PyTorch in the ~/Work/ml venv, the adam-torch-ref pattern: fixed seeds, committed fixture with provenance) for the full attention-bearing block forward AND a training-step trace, and assert against it.

Claim: agent=blockgold workspace=.jj-ws/fable-blockgold machine=spark (owns the external PyTorch golden fixture + its assert block in gptblock-attn-test.f; NOTE the tie-in-block lane concurrently edits the same test file - orchestrator hand-merges)
