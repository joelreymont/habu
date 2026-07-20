---
title: Adopt nanoGPT weight-decay coefficient in the tensor trainer
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-20T09:33:23.762403+02:00\""
---

Residual from the AdamW landing (c1255380, dot habu-adamw-decoupled-weight-d322fe1f): the tensor trainer now carries the real param-group policy (W1/W2 WD-DECAY, B1/B2 WD-NONE) but AMT-WD is pinned to 0.0 (maki/adam-train.f:107-113) so the committed AMT/ATN convergence locks (-2749 / 9 milli) stay bit-identical. Consequence: no committed training path exercises wd>0 - the decoupled math is proven only by the golden suite (maki/adamw-test.f), not by a training trajectory. Do: set the coefficient to nanoGPT's 0.1 on the decaying groups, RE-DERIVE the convergence locks honestly at the new trajectory (measure, do not relax), and prove the wd>0 run still converges deterministically and no worse than wd=0 on the fixture. If wd>0 measurably hurts this small MLP fixture, that is a valid recorded negative - keep 0.0 with the MEASURED justification in place of today's lock-preservation rationale. Territory: maki/adam-train.f + its fixtures.

Claim: agent=adamw2 workspace=.jj-ws/fable-adamw2 machine=spark (owns maki/adam-train.f + its fixtures; cosine-lr/weight-init/checkpoint/global-norm stay serialized behind this lane)
