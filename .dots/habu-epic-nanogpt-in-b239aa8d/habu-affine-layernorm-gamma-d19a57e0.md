---
title: Affine LayerNorm (gamma/beta) forward+backward+op
status: open
priority: 1
issue-type: task
created-at: "2026-07-18T15:24:38.427278+02:00"
---

GPT-2 LayerNorm has affine params; maki LN is NO-affine. Evidence: maki/layernorm.f LN-FWD/LN-BWD have no gamma/beta; cad.f OP-LAYERNORM is unary (SHP-LEGAL? gates it as 0-param). Add affine golden y=gamma*xhat+beta, its VJP (dgamma=sum(dy*xhat), dbeta=sum(dy), dx via existing LN-BWD chain), and either extend the LAYERNORM op to carry gamma/beta operands or compose LAYERNORM->SCALE->BIAS with correct adjoints. Small golden first (current idiom OK to de-risk), SPEC: rewrite later. Dep: none (layernorm.f exists).

2026-07-19 STAGE 1 LANDED (996b8ac3): LN-AFFINE-FWD/LN-AFFINE-BWD golden + fd-checked VJP in maki/layernorm.f (E-LN-DIM -5432; dgamma/dbeta ~1e-13, dx ~1e-5 vs central FD; corrupted-gradient detection 0.34 rel-L2), suite maki/layernorm-affine-test.f wired into maki/test.f; LN-FWD/LN-BWD byte-unchanged. ARCHITECTURE DECIDED: extend OP-LAYERNORM to carry gamma/beta operands (composition falsified: SHP-SCALE-OK? broadcast class is 1x1-or-same-shape, cannot express per-channel 1xC gamma; the 1xC broadcast op is habu-add-1xc-broadcast-5bef9d24's scope). REMAINING: op integration — cad capture arity 1->3 + SHP-LEGAL? 1xC checks for gamma/beta, adjoint row, BW-STEP arm emitting dgamma/dbeta/dx, plan-ops composer, executor + device lowering, training-fixture proof.
