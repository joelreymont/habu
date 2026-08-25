---
title: nanoGPT weight-init policy + Gaussian RNG
status: closed
priority: 2
issue-type: task
created-at: "2026-07-18T15:24:38.472605+02:00"
closed-at: "2026-07-20T10:28:59.339204+02:00"
close-reason: "Landed f12d1842: SC-GAUSS Gaussian sampler (polar Marsaglia over the existing LCG - no trig words exist in tree, polar needs only FLN+fsqrt; s=0 excluded so FLN never sees 0, red-first proven) + INIT-FILL role-based init policy (explicit checked roles: normal 0.02, residual 0.02/sqrt(2*n_layer), LN gamma=1/beta=0, bias=0; unknown role throws E-INIT-ROLE -5158). Deterministic statistical goldens at seed B5C0FFEE N=8192: mean 1.24e-4, var 4.0227e-4 vs 4e-4 target; goldens proven to detect non-Gaussianity (uniform swap reds 5 cases). New surface only - no default flipped, convergence locks bit-identical. Wiring into real block init belongs to the GPT-2 block dot; LCG triplication cleanup is habu-factor-maki-random-f3dce839"
---

PARTIAL: current init is uniform LCG only (from-scratch-model.f SC-FILL-SMALL [-0.1,0.1) via SC-UNIT; adam-train.f ATN-FILL). GPT-2 init is normal(0,0.02), residual-projection init scaled 0.02/sqrt(2*n_layer), LayerNorm gamma=1/beta=0, biases=0. Add a Gaussian RNG (Box-Muller over the existing LCG) and a per-parameter-role init policy word. Dep: LCG exists (from-scratch-model.f).

2026-07-20 SERIALIZED behind the adamw lane (spark): shared adam-train.f/trainer footprint.

2026-07-20 serialization RELEASED (adamw lane c1255380 + wd-adoption 8f8a09eb merged).
Claim: agent=winit workspace=.jj-ws/fable-winit machine=spark (owns maki/from-scratch-model.f + adam-train.f init path + new tests; cosine-lr/checkpoint/global-norm remain serialized behind this lane; must NOT touch adam-attn-grad-test.f - specpretty lane owns it)
