---
title: "Fix model DSL: multi-operand dataflow (Q@K^T) via SPEC:"
status: open
priority: 1
issue-type: task
created-at: "2026-07-18T16:40:36.105397+02:00"
blocks:
  - habu-spec-word-generating-0729fbea
---

Inventory finding (docs/nanogpt-inventory.md): MODEL: is single-running-value and 2-D-only — provably cannot express an internal Q@K^T (adam-train.f:15); residual skips only work via >V/named-ref workaround. THE FIX IS SPEC: (the load-bearing surface): multi-operand contractions are native to the spec grammar (A[ix[m] k] B[n k] form), so model compositions (MHA: habu-multi-head-self-a1e0692f, block: habu-gpt-2-block-a9039501) are AUTHORED as SPEC: lines, not MODEL: extensions — do NOT grow MODEL:'s running-value design. Scope here: (1) SPEC: grammar must cover the inventory's requirement list (contractions incl. transposed operands, gather indexing, reductions, broadcasts — docs/nanogpt-inventory.md SPEC-grammar section); (2) rank: (B,T,C) batch dimension enters via extent-typed tensors (coordinate habu-batch-sequence-tensor-006f25a1 — batch as an extent role, not a new tensor kind); (3) MODEL: stays for what it already does until SPEC: parity, then compositions migrate; deprecation decision AFTER nanoGPT trains. Acceptance: attention fwd expressed as SPEC: lines type-checks and matches maki/attention.f golden.
