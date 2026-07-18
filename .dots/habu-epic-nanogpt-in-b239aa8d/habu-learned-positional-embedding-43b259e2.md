---
title: Learned positional embedding + token+pos embed composition
status: open
priority: 1
issue-type: task
created-at: "2026-07-18T15:24:38.447136+02:00"
---

GPT-2 does wte[idx]+wpe[pos]. GATHER op exists (op-kind.f:40, cad.f OP-LOOKUP GATHER, lower-mv-test MODEL: GA) with SCATTER-ADD adjoint; embedding.f is the buffer golden (EMB-GATHER/EMB-SCATTER-ADD). MISSING: the wpe piece (positions 0..T-1: a SLICE of a TxC pos table, or a pos GATHER) and the token+pos elementwise ADD composition producing the block input. Prototype golden now; final form is a SPEC:/MODEL: composition. Dep: GATHER op + embedding.f exist; composition rides SPEC: chain.
