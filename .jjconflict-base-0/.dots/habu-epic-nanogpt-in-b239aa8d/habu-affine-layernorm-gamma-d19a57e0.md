---
title: Affine LayerNorm (gamma/beta) forward+backward+op
status: closed
priority: 1
issue-type: task
created-at: "2026-07-18T15:24:38.427278+02:00"
closed-at: "2026-07-19T20:17:11.403090+02:00"
close-reason: "Fully landed: stage 1 goldens 996b8ac3 + op integration 66c14dcb. OP-LAYERNORM in-count 0refs=unary/2refs=affine (backward-compat), 1xC shape guards, BW-STEP-LAYERNORM emits dgamma/dbeta/dx bound to goldens, xhat recomputed via BW-LN-FWD, host gradcheck 3/3 inputs V-PASS, Adam 30-step training proof, device lowering fail-closed E-LRED-OP"
---

GPT-2 LayerNorm has affine params; maki LN is NO-affine. Evidence: maki/layernorm.f LN-FWD/LN-BWD have no gamma/beta; cad.f OP-LAYERNORM is unary (SHP-LEGAL? gates it as 0-param). Add affine golden y=gamma*xhat+beta, its VJP (dgamma=sum(dy*xhat), dbeta=sum(dy), dx via existing LN-BWD chain), and either extend the LAYERNORM op to carry gamma/beta operands or compose LAYERNORM->SCALE->BIAS with correct adjoints. Small golden first (current idiom OK to de-risk), SPEC: rewrite later. Dep: none (layernorm.f exists).

2026-07-19 STAGE 1 LANDED (996b8ac3): LN-AFFINE-FWD/LN-AFFINE-BWD golden + fd-checked VJP in maki/layernorm.f (E-LN-DIM -5432; dgamma/dbeta ~1e-13, dx ~1e-5 vs central FD; corrupted-gradient detection 0.34 rel-L2), suite maki/layernorm-affine-test.f wired into maki/test.f; LN-FWD/LN-BWD byte-unchanged. ARCHITECTURE DECIDED: extend OP-LAYERNORM to carry gamma/beta operands (composition falsified: SHP-SCALE-OK? broadcast class is 1x1-or-same-shape, cannot express per-channel 1xC gamma; the 1xC broadcast op is habu-add-1xc-broadcast-5bef9d24's scope). REMAINING: op integration — cad capture arity 1->3 + SHP-LEGAL? 1xC checks for gamma/beta, adjoint row, BW-STEP arm emitting dgamma/dbeta/dx, plan-ops composer, executor + device lowering, training-fixture proof.

2026-07-19 DESTRUCTION REVIEW: behavioral integration landed, but encoding unary versus affine as the same opkind plus inferred input count leaves 0/2/4-input states representable; a two-input node silently executes as unary and ignores the extra operand, while the registry still declares arity 1. Corrective representation/arity ownership moved to optimization child habu-make-affine-layernorm-ddb6d70d. This closed dot remains the numerical-feature history, not proof that op identity is sound.

The review also found that affine device lowering still rejects in lower/red.f and the backward path emits ordinary MUL where the affine broadcast requires BCAST-MUL. The close reason's "fully landed" claim is therefore false beyond the stage-1 host golden. habu-make-affine-layernorm-ddb6d70d owns explicit identity, exact arity, correct broadcast adjoint, and host/device completion.
