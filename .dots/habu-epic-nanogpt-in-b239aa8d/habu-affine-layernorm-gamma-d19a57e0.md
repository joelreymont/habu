---
title: Affine LayerNorm (gamma/beta) forward+backward+op
status: open
priority: 1
issue-type: task
created-at: "2026-07-18T15:24:38.427278+02:00"
---

GPT-2 LayerNorm has affine params; maki LN is NO-affine. Evidence: maki/layernorm.f LN-FWD/LN-BWD have no gamma/beta; cad.f OP-LAYERNORM is unary (SHP-LEGAL? gates it as 0-param). Add affine golden y=gamma*xhat+beta, its VJP (dgamma=sum(dy*xhat), dbeta=sum(dy), dx via existing LN-BWD chain), and either extend the LAYERNORM op to carry gamma/beta operands or compose LAYERNORM->SCALE->BIAS with correct adjoints. Small golden first (current idiom OK to de-risk), SPEC: rewrite later. Dep: none (layernorm.f exists).
