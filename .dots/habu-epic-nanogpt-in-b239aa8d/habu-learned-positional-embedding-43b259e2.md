---
title: Learned positional embedding + token+pos embed composition
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-07-18T15:24:38.447136+02:00\\\"\""
closed-at: "2026-07-19T23:20:21.639145+02:00"
close-reason: "Landed 762165df-era: WPE-SLICE buffer golden (slice of MaxTxC, pad-scatter adjoint, E-WPE-EXTENT reject) + TOK-POS MODEL: composition (GATHER ADD) training with gradients reaching BOTH wte (scatter-add) and wpe (seed cotangent), Adam 0.2306->0.0037. SLICE chosen over pos-GATHER (simpler contiguous adjoint, both V-PASS today). DSL FINDING: single-running-value MODEL: cannot root two independent lookups (proven -5163/-5029) - full slice composition rides the SPEC: chain per the dot"
---

GPT-2 does wte[idx]+wpe[pos]. GATHER op exists (op-kind.f:40, cad.f OP-LOOKUP GATHER, lower-mv-test MODEL: GA) with SCATTER-ADD adjoint; embedding.f is the buffer golden (EMB-GATHER/EMB-SCATTER-ADD). MISSING: the wpe piece (positions 0..T-1: a SLICE of a TxC pos table, or a pos GATHER) and the token+pos elementwise ADD composition producing the block input. Prototype golden now; final form is a SPEC:/MODEL: composition. Dep: GATHER op + embedding.f exist; composition rides SPEC: chain.

Claim: agent=posembed workspace=.jj-ws/posembed machine=spark
