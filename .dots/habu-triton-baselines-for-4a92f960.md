---
title: Triton baselines for multi-op fusion comparison
status: open
priority: 2
issue-type: task
created-at: "2026-07-15T20:12:23.589798+02:00"
---

Leg (d) of habu-automatic-aggressive-fusion-828cdeb3, split out at its 2026-07-15 closure. USER-GATED (E2): the strict beat-Triton GB/s claim needs hand-fused Triton baselines for the same multi-op workloads (elementwise chain, layernorm, later fused-attention) on the Orin. No Triton toolchain is installed on zed and installing one is a user decision. When the user provides baseline numbers (or approves an install), import them via the typed BENCH surface (tools/eval-triton.f precedent - policy-comparable rows only, no incomparable-policy pairs) and add the comparison rows next to the landed fused-vs-ablated evidence (op-fusion 2.07x, layernorm 1.41x). Acceptance: committed comparable Triton rows + Habu-vs-Triton verdict rows, replay byte-stable. Ownership: ptx competitive evidence; blocked on user.
